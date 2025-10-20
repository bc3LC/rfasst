#' m3_get_pm25_ecoloss_vsl
#'
#' Produce economic damages associated with premature mortality attributable to PM2.5 exposure based on the IER functions from Burnett et al (2014), consistent with the GBD 2016 study. The economic valuation takes as a base value the widely accepted Value of Statistical Life (VSL) of the OECD for 2005. This value, according to the literature ranges between US$1.8 and $4.5 million. The calculations for all regions are based on the  “unit value transfer approach” which adjusts the VSL according to their GDP and GDP growth rates. (Markandya et al 2018)
#' @source Narain, U. and Sall, C., 2016. Methodology for Valuing the Health Impacts of Air Pollution//// Markandya, A., Sampedro, J., Smith, S.J., Van Dingenen, R., Pizarro-Irizar, C., Arto, I. and González-Eguino, M., 2018. Health co-benefits from air pollution and mitigation costs of the Paris Agreement: a modelling study. The Lancet Planetary Health, 2(3), pp.e126-e133.
#' @keywords module_3, VSL ,premature mortality, PM2.5
#' @return Economic damages associated with mortality attributable to PM2.5 exposure for each TM5-FASST regions for all years (Million$2015). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_model Select the health impact model (GBD, GEMM, or FUSION). By default = GBD
#' @param Damage_vsl_range Select the VSL to calculate the damages (VSL_med, VSL_low, or VSL_high). By default = VSL_med
#' @param inc_elas_vsl Select the income elasticity. Normally c(0.8, 1, 1.2). By default = 0.8
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @param downscale If set to T, produces gridded PM2.5 outputs and plots By default=F
#' @param saveRaster_grid If set to T, writes the raster file with weighted PM25 By default=F
#' @param agg_grid Re-aggregate (downscaled) gridded data to any provided geometries (shape file). For the moment, only "NUTS3" available
#' @param save_AggGrid If set to T, writes the raster file with the reaggregated PM25 By default=F
#' @param gcam_eur If set to T, considers the GCAM-Europe regions. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_pm25_ecoloss_vsl<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name, prj = NULL,
                                  scen_name, queries = "queries_rfasst.xml", ssp = "SSP2", final_db_year = 2100,
                                  mort_model = "GBD", Damage_vsl_range = "VSL_med", inc_elas_vsl = 0.8,
                                  saveOutput = T, map = F, anim = T, recompute = F, gcam_eur = F,
                                  downscale = F, saveRaster_grid = F,
                                  agg_grid = F, save_AggGrid = F){

  if (!recompute & exists('m3_get_pm25_ecoloss_vsl.output')) {
    return(m3_get_pm25_ecoloss_vsl.output)
  } else {

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_pm25_mort_ecoloss")) dir.create("output/maps/m3/maps_pm25_mort_ecoloss")

    # Ancillary Functions
    `%!in%` = Negate(`%in%`)

    # Shape subset for maps
    fasstSubset <- rmap::mapCountries

    fasstSubset<-as.data.frame(fasstSubset) %>%
      dplyr::mutate(subRegionAlt=as.character(subRegionAlt)) %>%
      dplyr::left_join(rfasst::fasst_reg, by = "subRegionAlt") %>%
      dplyr::select(-subRegion) %>%
      dplyr::rename(subRegion = fasst_region) %>%
      dplyr::mutate(subRegionAlt = as.factor(subRegionAlt))

    # Get Mortalities
    pm.mort<-m3_get_mort_pm25(db_path = db_path, db_name = db_name, prj_name = prj_name,
                              prj = prj, scen_name = scen_name, query_path = query_path,
                              queries = queries, ssp = ssp, saveOutput = F,
                              final_db_year = final_db_year, recompute = recompute,
                              gcam_eur = gcam_eur,
                              downscale = downscale, saveRaster_grid = saveRaster_grid,
                              agg_grid = agg_grid, save_AggGrid = save_AggGrid) %>%
      tidyr::pivot_longer(cols = c("GBD", "GEMM", "FUSION"),
                          names_to = "model",
                          values_to = "mort_pm25") %>%
      dplyr::filter(model == mort_model) %>%
      dplyr::group_by(region, year, scenario, sex) %>%
      dplyr::summarise(mort_pm25 = sum(mort_pm25)) %>%
      dplyr::ungroup()

    all_years<-all_years[all_years <= min(final_db_year,
                                          max(as.numeric(as.character(unique(pm.mort$year)))))]

    # Get gdp_pc
    gdp_pc<- get(
      if (downscale) { paste0('gdp_pc.ctry_nuts3.', ssp)
      } else { paste0('gdp_pc.', ssp)
      }, envir = asNamespace("rfasst")
    )

    #------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------
    # Economic valuation of premature mortality: Value of Statistical Life (VSL)
    vsl <- gdp_pc %>%
      # calculate the adjustment factor
      dplyr::mutate(adj = (gdp_pc / gdp_eu_2005) ^ inc_elas_vsl) %>%
      # multiply the LB and the UB of the European VSL (in 2005$), and take the median value
      dplyr::mutate(vsl_lb = adj * vsl_eu_2005_lb,
                    vsl_ub = adj * vsl_eu_2005_ub,
                    vsl_med = (vsl_lb + vsl_ub) / 2) %>%
      dplyr::select(scenario, region, year, vsl_med, vsl_lb, vsl_ub) %>%
      dplyr::mutate(vsl_med = vsl_med * 1E-6,
                    vsl_lb = vsl_lb * 1E-6,
                    vsl_ub = vsl_ub * 1E-6) %>%
      dplyr::mutate(unit = "Million$2005") %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::filter(year %in% all_years)


    m3_get_pm25_ecoloss_vsl.output.list <- list()
    for (sc in scen_name) {

      #------------------------------------------------------------------------------------
      pm.mort.EcoLoss<-pm.mort %>%
        dplyr::filter(scenario == sc) %>%
        dplyr::select(-scenario) %>%
        # some NUTS3 are not present in the VSL data - TODO: find or estimate them
        dplyr::left_join(vsl, by=c("region", "year")) %>%
        dplyr::filter(rowSums(is.na(.)) == 0) %>%
        dplyr::select(-scenario) %>%
        # Calculate the median damages
        dplyr::mutate(VSL_med = round(mort_pm25 * vsl_med * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                      VSL_low = round(mort_pm25 * vsl_lb * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                      VSL_high = round(mort_pm25 * vsl_ub * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                      unit = "Million$2015") %>%
        dplyr::select(-vsl_med, -vsl_lb, -vsl_ub) %>%
        tidyr::pivot_longer(cols = c("VSL_med", "VSL_low", "VSL_high"),
                            names_to = "range",
                            values_to = "damages") %>%
        dplyr::filter(range == Damage_vsl_range) %>%
        dplyr::select(region, year, sex, range, damages, unit) %>%
        dplyr::distinct()


      #------------------------------------------------------------------------------------
      # Write the output

      pm.mort.EcoLoss.list<-pm.mort.EcoLoss %>%
        dplyr::mutate(scenario = sc)
      m3_get_pm25_ecoloss_vsl.output.list <- append(m3_get_pm25_ecoloss_vsl.output.list, list(pm.mort.EcoLoss.list))
    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Bind the results

    m3_get_pm25_ecoloss_vsl.output <- dplyr::bind_rows(m3_get_pm25_ecoloss_vsl.output.list)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If saveOutput=T,  writes aggregated PM2.5 values per TM5-FASST region


    pm.mort.EcoLoss.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","PM25_MORT_ECOLOSS_VSL_",paste(scen_name, collapse = "-"),"_",unique(df$year),".csv"),row.names = F)
    }


    if(saveOutput == T){

      pm.mort.EcoLoss.list<-split(m3_get_pm25_ecoloss_vsl.output,m3_get_pm25_ecoloss_vsl.output$year)

      lapply(pm.mort.EcoLoss.list, pm.mort.EcoLoss.write)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If map=T, it produces a map with the calculated outcomes

    if(map && !downscale){

      pm.mort.EcoLoss.map <- m3_get_pm25_ecoloss_vsl.output %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, damages, unit, scenario) %>%
        dplyr::rename(value = damages,
                      units = unit) %>%
        dplyr::mutate(year = as.numeric(as.character(year)),
                      value = value * 1E-6,
                      units = "Trillion$2015")

      rmap::map(data = pm.mort.EcoLoss.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_pm25_mort_ecoloss",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)
    } else if(map && downscale){

      ctry_nuts_sf <- rfasst::ctry_nuts_sf
      ctry_nuts <- as(ctry_nuts_sf, "SpatVector")
      ctry_nuts <- ctry_nuts %>%
        dplyr::left_join(m3_get_pm25_ecoloss_vsl.output %>%
                           dplyr::rename(id_code = region,
                                         value = damages) %>%
                           dplyr::mutate(year = as.numeric(as.character(year)),
                                         value = value * 1E-6,
                                         units = "Trillion$2015") %>%
                           tibble::as_tibble(),
                         by = 'id_code')

      # Crop to the European region
      nuts_europe <- ctry_nuts %>%
        dplyr::filter(id_code %in% (rfasst::nuts_europe_sf %>%
                                      dplyr::pull(id_code)))

      for (y in unique(m3_get_pm25_ecoloss_vsl.output$year)) {

        # Global
        pm.mort.EcoLoss.map <- ggplot2::ggplot(data = ctry_nuts %>%
                                                 dplyr::filter(year == y, sex == 'Both')) +
          tidyterra::geom_spatvector(ggplot2::aes(fill = value), size = 0.1) +
          ggplot2::scale_fill_distiller(palette = "OrRd", direction = 1, name = "Trillion$2015") +
          ggplot2::theme_bw()

        ggplot2::ggsave(paste0(here::here(),"/output/maps/m3/maps_pm25_mort_ecoloss/", y,"_mort_ecoloss.pdf"), pm.mort.EcoLoss.map,
                        width = 500, height = 400, units = 'mm')

        # Europe
        pm.mort.EcoLoss.nuts3.map <- ggplot2::ggplot(data = nuts_europe %>%
                                               dplyr::filter(year == y, sex == 'Both')) +
          tidyterra::geom_spatvector(ggplot2::aes(fill = value), size = 0.1) +
          ggplot2::scale_fill_distiller(palette = "OrRd", direction = 1, name = "Trillion$2015") +
          ggplot2::theme_bw()

        ggplot2::ggsave(paste0(here::here(),"/output/maps/m3/maps_pm25_mort_ecoloss/", y,"_EUR-NUTS3_mort_ecoloss.pdf.pdf"), pm.mort.EcoLoss.nuts3.map,
                        width = 500, height = 300, units = 'mm')


      }
      cat('Maps saved at output/maps/m3/maps_pm25_mort_ecoloss')

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output

    return(invisible(m3_get_pm25_ecoloss_vsl.output))
  }

}

#' m3_get_o3_ecoloss_vsl
#'
#' Produce economic damages associated with premature mortality attributable to O3 (M6M) exposure based on the IER functions from Jerret et al (2009), consistent with the GBD 2016 study. The economic valuation takes as a base value the widely accepted Value of Statistical Life (VSL) of the OECD for 2005. This value, according to the literature ranges between US$1.8 and $4.5 million. The calculations for all regions are based on the  “unit value transfer approach” which adjusts the VSL according to their GDP and GDP growth rates. (Markandya et al 2018)
#' @source Jerrett, M., Burnett, R.T., Pope III, C.A., Ito, K., Thurston, G., Krewski, D., Shi, Y., Calle, E. and Thun, M., 2009. Long-term ozone exposure and mortality. New England Journal of Medicine, 360(11), pp.1085-1095.//// Markandya, A., Sampedro, J., Smith, S.J., Van Dingenen, R., Pizarro-Irizar, C., Arto, I. and González-Eguino, M., 2018. Health co-benefits from air pollution and mitigation costs of the Paris Agreement: a modelling study. The Lancet Planetary Health, 2(3), pp.e126-e133.
#' @keywords module_3, VSL ,premature mortality, O3
#' @return Economic damages associated with mortality attributable to O3 (M6M) exposure for each TM5-FASST regions for all years (Million$2015). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_model Select the health function (GBD2016 or Jerret2009). By default = Jerret2009
#' @param Damage_vsl_range Select the VSL to calculate the damages (VSL_med, VSL_low, or VSL_high). By default = VSL_med
#' @param inc_elas_vsl Select the income elasticity. Normally c(0.8, 1, 1.2). By default = 0.8
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @param gcam_eur If set to T, considers the GCAM-Europe regions. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_o3_ecoloss_vsl<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name, prj = NULL,
                                 scen_name, ssp = "SSP2", final_db_year = 2100,
                                 mort_model = "Jerret2009",  Damage_vsl_range = "VSL_med", inc_elas_vsl = 0.8,
                                 queries = "queries_rfasst.xml", saveOutput = T, map = F, anim = T, recompute = F, gcam_eur = F){

  if (!recompute & exists('m3_get_o3_ecoloss_vsl.output')) {
    return(m3_get_o3_ecoloss_vsl.output)
  } else {
    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_o3_mort_ecoloss")) dir.create("output/maps/m3/maps_o3_mort_ecoloss")

    # Ancillary Functions
    `%!in%` = Negate(`%in%`)

    # Shape subset for maps
    fasstSubset <- rmap::mapCountries

    fasstSubset<-as.data.frame(fasstSubset) %>%
      dplyr::mutate(subRegionAlt=as.character(subRegionAlt)) %>%
      dplyr::left_join(rfasst::fasst_reg,by="subRegionAlt") %>%
      dplyr::select(-subRegion) %>%
      dplyr::rename(subRegion=fasst_region) %>%
      dplyr::mutate(subRegionAlt=as.factor(subRegionAlt))

    # Get Mortalities
    o3.mort <- m3_get_mort_o3(db_path = db_path, db_name = db_name, prj_name = prj_name, prj = prj, scen_name = scen_name, query_path = query_path,
                            queries = queries, ssp = ssp, saveOutput = F, final_db_year = final_db_year, recompute = recompute, gcam_eur = gcam_eur) %>%
      tidyr::pivot_longer(cols = c("Jerret2009", "GBD2016"),
                          names_to = "model",
                          values_to = "mort_o3") %>%
      dplyr::filter(model == mort_model) %>%
      dplyr::group_by(region, year, scenario) %>%
      dplyr::summarise(mort_o3 = sum(mort_o3)) %>%
      dplyr::ungroup()

    all_years<-all_years[all_years <= min(final_db_year,
                                          max(as.numeric(as.character(unique(o3.mort$year)))))]

    # Get gdp_pc
    gdp_pc<-get(paste0('gdp_pc.',ssp))

    #------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------
    # Economic valuation of premature mortality: Value of Statistical Life (VSL)
    vsl <- gdp_pc %>%
      # calculate the adjustment factor
      dplyr::mutate(adj = (gdp_pc / gdp_eu_2005) ^ inc_elas_vsl) %>%
      # multiply the LB and the UB of the European VSL (in 2005$), and take the median value
      dplyr::mutate(vsl_lb = adj * vsl_eu_2005_lb,
                    vsl_ub = adj * vsl_eu_2005_ub,
                    vsl_med = (vsl_lb + vsl_ub) / 2) %>%
      dplyr::select(scenario, region, year, vsl_med, vsl_lb, vsl_ub) %>%
      dplyr::mutate(vsl_med = vsl_med * 1E-6,
                    vsl_lb = vsl_lb * 1E-6,
                    vsl_ub = vsl_ub * 1E-6) %>%
      dplyr::mutate(unit = "Million$2005") %>%
      dplyr::mutate(year = as.numeric(year))


    m3_get_o3_ecoloss_vsl.output.list <- list()
    for (sc in scen_name) {

      #------------------------------------------------------------------------------------
      o3.mort.EcoLoss<-o3.mort %>%
        dplyr::filter(scenario == sc) %>%
        dplyr::select(-scenario) %>%
        gcamdata::left_join_error_no_match(vsl, by = c("region","year")) %>%
        dplyr::select(-scenario) %>%
        # Calculate the median damages
        dplyr::mutate(VSL_med = round(mort_o3 * vsl_med * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                      VSL_low = round(mort_o3 * vsl_lb * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                      VSL_high = round(mort_o3 * vsl_ub * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                      unit = "Million$2015") %>%
        dplyr::select(-vsl_med, -vsl_lb, -vsl_ub) %>%
        tidyr::pivot_longer(cols = c("VSL_med", "VSL_low", "VSL_high"),
                            names_to = "range",
                            values_to = "damages") %>%
        dplyr::filter(range == Damage_vsl_range) %>%
        dplyr::select(region, year, range, damages, unit)


      #------------------------------------------------------------------------------------
      # Write the output

      o3.mort.EcoLoss.list<-o3.mort.EcoLoss %>%
        dplyr::mutate(scenario = sc)
      m3_get_o3_ecoloss_vsl.output.list <- append(m3_get_o3_ecoloss_vsl.output.list, list(o3.mort.EcoLoss.list))
    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Bind the results

    m3_get_o3_ecoloss_vsl.output <- dplyr::bind_rows(m3_get_o3_ecoloss_vsl.output.list)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If saveOutput=T,  writes aggregated PM2.5 values per TM5-FASST region


    o3.mort.EcoLoss.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","O3_MORT_ECOLOSS_VSL_",paste(scen_name, collapse = "-"),"_",unique(df$year),".csv"),row.names = F)
    }


    if(saveOutput == T){

      o3.mort.EcoLoss.list<-split(m3_get_o3_ecoloss_vsl.output, m3_get_o3_ecoloss_vsl.output$year)

      lapply(o3.mort.EcoLoss.list, o3.mort.EcoLoss.write)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    # If map=T, it produces a map with the calculated outcomes

    if(map == T){

      o3.mort.EcoLoss.map <- m3_get_o3_ecoloss_vsl.output %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, damages, scenario) %>%
        dplyr::rename(value = damages) %>%
        dplyr::mutate(year = as.numeric(as.character(year)),
                      units = "Trillion$2015",
                      value = value * 1E-6)

      rmap::map(data = o3.mort.EcoLoss.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_o3_mort_ecoloss",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)

    }
    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output

    return(invisible(m3_get_o3_ecoloss_vsl.output))
  }

}

#' m3_get_pm25_ecoloss_gdpGrowth
#'
#'
#' Produce reduced GDP growth related to PM2.5 concentration.
#' @source Dong, D., Xu, B., Shen, N. and He, Q., 2021. The adverse impact of air pollution on China’s economic growth. Sustainability, 13(16), p.9056.
#' @keywords module_3, GDP growth, PM2.5
#' @return Impact on GDP growth associated with PM2.5 exposure for each TM5-FASST regions for all years (%). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_model Select the health impact model (GBD, GEMM, or FUSION). By default = GBD
#' @param inc_elas Select the income elasticity. Normally c(0.8, 1, 1.2). By default = 0.8
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @param gcam_eur If set to T, considers the GCAM-Europe regions. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_pm25_ecoloss_gdpGrowth<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name, prj = NULL,
                                  scen_name, ssp = "SSP2", final_db_year = 2100,
                                  mort_model = "GBD", inc_elas = 0.8,
                                  queries = "queries_rfasst.xml", saveOutput = T, map = F, anim = T, recompute = F, gcam_eur = F){

  if (!recompute & exists('m3_get_pm25_ecoloss_gdpGrowth.output')) {
    return(m3_get_pm25_ecoloss_gdpGrowth.output)
  } else {
    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_pm25_mort_ecoloss")) dir.create("output/maps/m3/maps_pm25_mort_ecoloss")

    # Ancillary Functions
    `%!in%` = Negate(`%in%`)

    # Shape subset for maps
    fasstSubset <- rmap::mapCountries

    fasstSubset<-as.data.frame(fasstSubset) %>%
      dplyr::mutate(subRegionAlt=as.character(subRegionAlt)) %>%
      dplyr::left_join(rfasst::fasst_reg, by = "subRegionAlt") %>%
      dplyr::select(-subRegion) %>%
      dplyr::rename(subRegion = fasst_region) %>%
      dplyr::mutate(subRegionAlt = as.factor(subRegionAlt))

    # Get Mortalities
    pm.conc <- m2_get_conc_pm25(db_path, query_path, db_name, prj_name, prj = prj, scen_name, queries, saveOutput = F,
                             final_db_year = final_db_year, recompute = recompute, gcam_eur = gcam_eur)

    all_years<-all_years[all_years <= min(final_db_year,
                                          max(as.numeric(as.character(unique(pm.conc$year)))))]

    # Get gdp_pc
    gdp_pc<-get(paste0('gdp_pc.',ssp))

    gdp_pc_chn <- gdp_pc %>%
      dplyr::filter(year == 2015,
                    region == "CHN")

    chn.gdppc.2015 <- gdp_pc_chn$gdp_pc * gcamdata::gdp_deflator(2015, 2005)


    # Economic valuation of premature mortality: Value of Statistical Life (VSL)
    beta <- gdp_pc %>%
      dplyr::mutate(beta_chn = beta_dongetal2021) %>%
      # calculate the adjustment factor
      dplyr::mutate(adj = (gdp_pc / chn.gdppc.2015) ^ inc_elas) %>%
      # multiply the beta by the adjustment factor
      dplyr::mutate(beta_adj = beta_chn * adj) %>%
      dplyr::select(scenario, region, year, beta = beta_adj) %>%
      dplyr::select(-scenario) %>%
      tidyr::complete(tidyr::nesting(region), year = all_years) %>%
      dplyr::group_by(region) %>%
      dplyr::mutate(year = as.numeric(as.character(year))) %>%
      dplyr::mutate(beta = gcamdata::approx_fun(year, beta, rule = 2)) %>%
      dplyr::ungroup()



    m3_get_pm25_ecoloss_gdpGrowth.output.list <- list()
    for (sc in scen_name) {

      #------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------
      pm.mort.EcoLoss.gdpGrowth <- tibble::as_tibble(pm.conc %>%
                                                       dplyr::filter(scenario == sc)) %>%
        dplyr::mutate(year = as.numeric(as.character(year))) %>%
        gcamdata::left_join_error_no_match(beta, by=c("region", "year")) %>%
        dplyr::mutate(damages = beta * value,
                      unit = "%") %>%
        dplyr::select(region, year, damages, unit)



      #------------------------------------------------------------------------------------
      # Write the output

      pm.mort.EcoLoss.gdpGrowth.list<-pm.mort.EcoLoss.gdpGrowth %>%
        dplyr::mutate(scenario = sc)
      m3_get_pm25_ecoloss_gdpGrowth.output.list <- append(m3_get_pm25_ecoloss_gdpGrowth.output.list, list(pm.mort.EcoLoss.gdpGrowth.list))

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Bind the results

    m3_get_pm25_ecoloss_gdpGrowth.output <- dplyr::bind_rows(m3_get_pm25_ecoloss_gdpGrowth.output.list)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If saveOutput=T,  writes aggregated PM2.5 values per TM5-FASST region

    pm.mort.EcoLoss.gdpGrowth.write<-function(df){
      df <- as.data.frame(df)
      write.csv(df,paste0("output/","m3/","PM25_MORT_ECOLOSS_GDPgrowth_",paste(scen_name, collapse = "-"),"_",unique(df$year),".csv"),row.names = F)
    }


    if(saveOutput == T){

      pm.mort.EcoLoss.gdpGrowth.list<-split(m3_get_pm25_ecoloss_gdpGrowth.output,m3_get_pm25_ecoloss_gdpGrowth.output$year)

      lapply(pm.mort.EcoLoss.gdpGrowth.list, pm.mort.EcoLoss.gdpGrowth.write)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If map=T, it produces a map with the calculated outcomes

    if(map == T){

      pm.mort.EcoLoss.map <- m3_get_pm25_ecoloss_gdpGrowth.output %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, damages, unit, scenario) %>%
        dplyr::rename(value = damages,
                      units = unit) %>%
        dplyr::mutate(year = as.numeric(as.character(year)))

      rmap::map(data = pm.mort.EcoLoss.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_pm25_mort_ecoloss",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)

    }
    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output

    return(invisible(m3_get_pm25_ecoloss_gdpGrowth.output))
  }

}




