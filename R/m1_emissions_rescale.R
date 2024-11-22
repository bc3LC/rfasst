##' m1_emissions_rescale
#'
#'
#' Re-scale emissions from GCAM regions to TM5-FASST regions. The function also completes some additional transformation required such as pollutant transformation (e.g. OC to POM) or unit changes (Tg to Kg).
#' @keywords module_1, re-scale emissions
#' @return Emissions per TM5-FASST region (Kg) for all the selected years (all_years). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param saveOutput Writes the emission files. By default=T
#' @param map Produce the maps. By default=F
#' @param mapIndivPol If set to T, it produces the maps for individual pollutants. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @importFrom magrittr %>%
#' @export

m1_emissions_rescale<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name, prj = NULL,
                               scen_name, queries = "queries_rfasst.xml", final_db_year = 2100,
                               saveOutput = T, map = F, mapIndivPol = F, anim = T, recompute = F){

  if (!recompute & exists('m1_emissions_rescale.output')) {
    return(m1_emissions_rescale.output)
  } else {

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(!is.null(prj_name) && !endsWith(prj_name, '.dat')) prj_name = paste0(prj_name, '.dat')
    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    all_years<-all_years[all_years <= final_db_year]

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m1")) dir.create("output/m1")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m1")) dir.create("output/maps/m1")
    if (!dir.exists("output/maps/m1/maps_em")) dir.create("output/maps/m1/maps_em")

    # Ancillary Functions
    `%!in%` = Negate(`%in%`)

    # Shape subset for maps
    fasstSubset <- rmap::mapCountries

    fasstSubset<-as.data.frame(fasstSubset) %>%
      dplyr::mutate(subRegionAlt = as.character(subRegionAlt)) %>%
      dplyr::left_join(rfasst::fasst_reg, by = "subRegionAlt") %>%
      dplyr::select(-subRegion) %>%
      dplyr::rename(subRegion = fasst_region) %>%
      dplyr::mutate(subRegionAlt = as.factor(subRegionAlt))

    # Transform the loaded TM5-FASST regions
    FASST_reg<-rfasst::fasst_reg %>%
      dplyr::rename(`ISO 3`=subRegionAlt,
                    `FASST Region`=fasst_region)

    # Load the rgcam project if prj not passed as a parameter:
    if (is.null(prj)) {
      if (!is.null(db_path) & !is.null(db_name)) {
        rlang::inform('Creating project ...')
        conn <- rgcam::localDBConn(db_path,
                                   db_name,migabble = FALSE)
        prj <- rgcam::addScenario(conn,
                                  prj_name,
                                  scen_name,
                                  paste0(query_path,"/",queries),
                                  saveProj = F)

        # add nonCO2 query manually (it is too big to use the usual method)
        if (!'nonCO2 emissions by sector (excluding resource production)' %in% rgcam::listQueries(prj)) {
          dt_sec <- data_query('nonCO2 emissions by sector (excluding resource production)',
                               db_path, db_name, prj_name, scen_name,
                               query_path, 'queries_rfasst_nonCO2.xml')
          prj_tmp <- rgcam::addQueryTable(project = prj_name, qdata = dt_sec,
                                          queryname = 'nonCO2 emissions by sector (excluding resource production)', clobber = FALSE)
          prj <- rgcam::mergeProjects(prj_name, list(prj,prj_tmp), clobber = TRUE, saveProj = FALSE)
          rm(prj_tmp); rm(dt_sec)
        }

        rgcam::saveProject(prj, file = file.path('output',prj_name))

        QUERY_LIST <- c(rgcam::listQueries(prj, c(scen_name)))
      } else {
        rlang::inform('Loading project ...')
        prj <- rgcam::loadProject(prj_name)

        QUERY_LIST <- c(rgcam::listQueries(prj, c(scen_name)))
      }
    } else {
      QUERY_LIST <- c(rgcam::listQueries(prj, c(scen_name)))
    }
    rlang::inform('Computing emissions ...')


    # Generate the adjusted emission database +air and ship emissions
    # Emission data (scen)
    scen_sct <- rgcam::getQuery(prj,"nonCO2 emissions by sector (excluding resource production)") %>%
      dplyr::filter(ghg %in% unique(levels(as.factor(rfasst::selected_pollutants)))) %>%
      tibble::as_tibble() %>%
      gcamdata::left_join_error_no_match(rfasst::my_pol %>%
                                           dplyr::rename(ghg = Pollutant) %>%
                                           tibble::as_tibble(), by = "ghg") %>%
      dplyr::mutate(ghg = dplyr::if_else(grepl("SO2", ghg), "SO2", ghg)) %>%
      dplyr::select(-ghg) %>%
      dplyr::rename(ghg = My_Pollutant) %>%
      dplyr::group_by(Units, scenario, region, sector, ghg, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()

    scen_rsc <- rgcam::getQuery(prj,"nonCO2 emissions by resource production") %>%
      dplyr::filter(ghg %in% unique(levels(as.factor(rfasst::selected_pollutants)))) %>%
      tibble::as_tibble() %>%
      gcamdata::left_join_error_no_match(rfasst::my_pol %>%
                                           dplyr::rename(ghg = Pollutant) %>%
                                           tibble::as_tibble(), by = 'ghg') %>%
      dplyr::mutate(ghg = dplyr::if_else(grepl("SO2", ghg), "SO2", ghg)) %>%
      dplyr::select(-ghg) %>%
      dplyr::rename(ghg = My_Pollutant) %>%
      dplyr::select(-subresource) %>%
      dplyr::rename(sector = resource) %>%
      dplyr::mutate(sector = "rsc production") %>%
      dplyr::group_by(Units, scenario, region, sector, ghg, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()

    scen<-dplyr::bind_rows(scen_sct, scen_rsc) %>%
      dplyr::filter(scenario %in% scen_name,
                    year <= final_db_year) %>%
      dplyr::filter(complete.cases(.))

    # Transform OC to POM
    pom<-scen %>%
      dplyr::filter(ghg %in% c("OC","OC_AWB")) %>%
      dplyr::mutate(value = dplyr::if_else(ghg=="OC_AWB", value * CONV_OCawb_POM, value * CONV_OC_POM)) %>%
      dplyr::mutate(ghg = "POM")

    # Transform MTC to MTCO2
    co2<-scen %>%
      dplyr::filter(ghg == "CO2") %>%
      dplyr::mutate(value = value * MTC_MTCO2)

    if(nrow(co2) == 0){

      co2 <- scen %>%
        dplyr::filter(ghg == "CH4") %>%
        dplyr::mutate(value = 0)

    }

    scen <- scen %>%
      dplyr::filter(ghg %!in% c("OC","OC_AWB","CO2")) %>%
      dplyr::filter(scenario %in% scen_name,
                    year <= final_db_year) %>%
      dplyr::bind_rows(pom,co2) %>%
      # transform value to kg
      dplyr::mutate(value=value*TG_KG) %>%
      dplyr::group_by(scenario,region,Units,ghg,year) %>%
      dplyr::summarise(value=sum(value))%>%
      dplyr::ungroup()%>%
      dplyr::select(-Units) %>%
      dplyr::filter(year %in% all_years) %>%
      dplyr::rename(`GCAM Region` = region,
                    Pollutant = ghg) %>%
      dplyr::mutate(Pollutant = as.factor(Pollutant),
                    year = as.factor(year),
                   `GCAM Region` = as.factor(`GCAM Region`))

    # Aviation:
    air <- rgcam::getQuery(prj,"International Aviation emissions") %>%
      dplyr::filter(scenario %in% scen_name,
                    year <= final_db_year) %>%
      dplyr::mutate(ghg = dplyr::if_else(grepl("SO2",ghg), "SO2", as.character(ghg))) %>%
      dplyr::group_by(scenario, ghg,year) %>%
      dplyr::summarise(value=sum(value)) %>%
      dplyr::ungroup() %>%
      # transform value to kg
      dplyr::mutate(value=value * TG_KG)  %>%
      dplyr::filter(year %in% all_years) %>%
      dplyr::mutate(ghg = as.factor(ghg)) %>%
      dplyr::rename(Pollutant = ghg) %>%
      dplyr::mutate(Pollutant = dplyr::if_else(Pollutant =="OC","POM", as.character(Pollutant)),
                    Pollutant = as.factor(Pollutant)) %>%
      tidyr::spread(Pollutant, value) %>%
      dplyr::mutate(CH4=rep(0),
                    N2O=rep(0),
                    NH3=rep(0))

    check_co2 <- as.data.frame(colnames(air)) %>%
      dplyr::rename(ghg = `colnames(air)`) %>%
      dplyr::filter(ghg == "CO2")

    if(nrow(check_co2) == 0){

      CO2 <- rep(0, nrow(air))

      air <- air %>%
        cbind(CO2)
    }

    air <- air %>%
      dplyr::select(scenario, year,BC,CH4,CO2,CO,N2O,NH3,NOx,POM,SO2,NMVOC)

    # Shipping:
    ship <- rgcam::getQuery(prj,"International Shipping emissions") %>%
      dplyr::filter(scenario %in% scen_name,
                    year <= final_db_year) %>%
      dplyr::mutate(ghg = dplyr::if_else(grepl("SO2",ghg),"SO2", as.character(ghg))) %>%
      dplyr::group_by(scenario, ghg, year) %>%
      dplyr::summarise(value=sum(value)) %>%
      dplyr::ungroup() %>%
      # transform value to kg
      dplyr::mutate(value = value * TG_KG) %>%
      dplyr::filter(year %in% all_years) %>%
      dplyr::mutate(ghg = as.factor(ghg)) %>%
      dplyr::rename(Pollutant = ghg) %>%
      dplyr::mutate(Pollutant = dplyr::if_else(Pollutant =="OC","POM", as.character(Pollutant)),
                    Pollutant = as.factor(Pollutant))%>%
      tidyr::spread(Pollutant,value) %>%
      dplyr::mutate(CH4=rep(0),
                    N2O=rep(0),
                    NH3=rep(0))

    check_co2_ship <- as.data.frame(colnames(ship)) %>%
      dplyr::rename(ghg = `colnames(ship)`) %>%
      dplyr::filter(ghg == "CO2")

    if(nrow(check_co2_ship) == 0){

      CO2 <- rep(0, nrow(ship))

      ship <- ship %>%
        cbind(CO2)
    }

    ship <- ship %>%
      dplyr::select(scenario,year,BC,CH4,CO2,CO,N2O,NH3,NOx,POM,SO2,NMVOC)

    final_df_wide<-dplyr::left_join(Percen %>%
                                      dplyr::filter(year %in% all_years,
                                                    Pollutant != "CO2") %>%
                                      dplyr::mutate(`GCAM Region`=as.factor(`GCAM Region`)),
                                    scen,
                                    by=c('GCAM Region','Pollutant','year')) %>%
      dplyr::mutate(NewValue = Percentage * value) %>%
      dplyr::left_join(FASST_reg, by = 'ISO 3') %>%
      dplyr::group_by(scenario, `FASST Region`, year, Pollutant) %>%
      dplyr::summarise(NewValue = sum(NewValue)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(Region=`FASST Region`,
                    Year=year,
                    value=NewValue) %>%
      tidyr::spread(Pollutant,value) %>%
      dplyr::mutate(CO2 = 0)


    if(length(levels(as.factor(FASST_reg$`FASST Region`))) != length(levels(as.factor(final_df_wide$Region)))){
      print("ISO 3 countries do not match")
    }


    #Write the data
    #Create the function to write the csv files

    write_data <- function(save = saveOutput){

      a <- final_df_wide %>%
        dplyr::rename(COUNTRY = Region,
                      NOX = NOx,
                      OM = POM,
                      VOC = NMVOC) %>%
        dplyr::select(scenario, Year, COUNTRY,BC,CH4,CO2,CO,N2O,NH3,NOX,OM,SO2,VOC)


      # Shipping and aviation
      vec_air<-air %>%
        dplyr::rename(Year = year) %>%
        dplyr::mutate(COUNTRY = "AIR") %>%
        dplyr::select(scenario, Year, COUNTRY, BC, CH4, CO2, CO, N2O, NH3, NOx, POM, SO2, NMVOC) %>%
        dplyr::rename(NOX = NOx,
                      OM = POM,
                      VOC = NMVOC)

      vec_ship<-ship %>%
        dplyr::rename(Year = year) %>%
        dplyr::mutate(COUNTRY = "SHIP") %>%
        dplyr::select(scenario, Year, COUNTRY, BC, CH4, CO2, CO, N2O, NH3, NOx, POM, SO2, NMVOC) %>%
        dplyr::rename(NOX = NOx,
                      OM = POM,
                      VOC = NMVOC)


      # Add RUE:
      rus<- a %>%
        dplyr::filter(COUNTRY == "RUS") %>%
        tidyr::gather(Pollutant, value, -COUNTRY, -scenario, -Year) %>%
        dplyr::mutate(COUNTRY = as.character(COUNTRY),
                      Pollutant = as.character(Pollutant))

      rue <- a %>%
        dplyr::filter(COUNTRY == "RUS") %>%
        tidyr::gather(Pollutant, value, -COUNTRY, -scenario, -Year) %>%
        dplyr::mutate(COUNTRY = "RUE") %>%
        dplyr::mutate(COUNTRY = as.character(COUNTRY),
                      Pollutant = as.character(Pollutant))


      rus_fin<-dplyr::bind_rows(rus, rue) %>%
        dplyr::mutate(COUNTRY = as.factor(COUNTRY),
                      Pollutant = as.factor(Pollutant)) %>%
        dplyr::left_join(adj_rus %>%
                           dplyr::mutate(COUNTRY = as.factor(COUNTRY)),
                         by=c("COUNTRY","Pollutant")) %>%
        dplyr::mutate(value = value * perc) %>%
        dplyr::select(-perc) %>%
        tidyr::spread(Pollutant, value) %>%
        dplyr::mutate(Year = as.numeric(as.character(Year)))


      # Add aviation, shipping and Russia
      a <- a %>%
        dplyr::mutate(Year = as.numeric(as.character(Year))) %>%
        dplyr::filter(COUNTRY != "RUS") %>%
        dplyr::bind_rows(vec_air, vec_ship, rus_fin %>% dplyr::mutate(COUNTRY = as.character(COUNTRY)))



      # Total
      tot <-a %>%
        tidyr::gather(Pollutant, value, -COUNTRY, -scenario, -Year) %>%
        dplyr::group_by(scenario, Year, Pollutant) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::ungroup() %>%
        tidyr::spread(Pollutant, value) %>%
        dplyr::mutate(COUNTRY = "*TOTAL*") %>%
        dplyr::select(scenario, Year, COUNTRY, BC, CH4, CO2, CO, N2O, NH3, NOX, OM, SO2, VOC)

      # Add total and PM2.5
      a <- a %>%
        dplyr::bind_rows(tot) %>%
        dplyr::mutate(PM25 = rep(-99900))


      if(save == T){
      write.csv(a, file = paste0("output/m1/", "emissions", '.csv'),row.names = FALSE, quote = FALSE)
      }

      em <- a %>%
        tidyr::gather(pollutant, value, -COUNTRY, -scenario, -Year) %>%
        dplyr::rename(region = COUNTRY) %>%
        dplyr::filter(region != "*TOTAL*")

      return(em)

    }

    # If map=T, it produces a map with the calculated outcomes

    final_df_wide.map <- final_df_wide %>%
      tidyr::gather(pollutant, value, -Region, -scenario, -Year) %>%
      dplyr::filter(pollutant %in% map_pol) %>%
      dplyr::rename(subRegion = Region)%>%
      dplyr::filter(subRegion != "RUE") %>%
      dplyr::select(scenario, subRegion, Year, pollutant, value) %>%
      dplyr::rename(year = Year) %>%
      dplyr::rename(class = pollutant) %>%
      dplyr::mutate(year = as.numeric(as.character(year)),
                    value = value * 1E-6,
                    units = "Gg")

    if(map==T & mapIndivPol == F){

      rmap::map(data = final_df_wide.map,
                shape = fasstSubset,
                folder ="output/maps/m1/maps_em",
                ncol = 3,
                legendType = "pretty",
                background = T,
                animate = anim)
    }

    if(map==T & mapIndivPol == T){

      final_df_wide.map.list<-split(final_df_wide.map,final_df_wide.map$class)

      for (df in final_df_wide.map.list) {
        rmap::map(data = df,
                  shape = fasstSubset,
                  folder =paste0("output/maps/m1/maps_em/",unique(df$class)),
                  legendType = "pretty",
                  background  = T,
                  save = T,
                  animate = anim)

      }

    }


    # Apply the function to all of the years.
    # This can be modified and write the data just for the desired years
    m1_emissions_rescale.output <<- write_data() %>%
      dplyr::rename(year = Year)
    return(invisible(m1_emissions_rescale.output))
  }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

}



