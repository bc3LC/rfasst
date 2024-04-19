#' calc_mort_rates
#'
#' Get cause-specific baseline mortalities from stroke, ischemic heart disease (IHD), chronic obstructive pulmonary disease (COPD), acute lower respiratory illness diseases (ALRI) and lung cancer (LC).
#' @source https://www.who.int/healthinfo/global_burden_disease/cod_2008_sources_methods.pdf
#' @keywords Baseline mortality rates
#' @return Baseline mortality rates for TM5-FASST regions for all years and causes (ALRI, COPD, LC, IHD, STROKE). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @importFrom magrittr %>%
#' @export

calc_mort_rates<-function(){
  mort.rates <- raw.mort.rates %>%
    dplyr::mutate(rate = dplyr::if_else(rate <= 0, 0, rate))

  invisible(mort.rates)
}




#' calc_daly_pm25
#'
#' Get the DALY-to-Mortality ratios used to estimate the Disability Adjusted Life Years attributable to fine particulate matter (PM2.5) exposure
#' @source Institute for Health Metrics and Evaluation (http://www.healthdata.org/)
#' @keywords DALYs, PM2.5
#' @return DALY-to-Mortality ratios for TM5-FASST regions for all years and PM2.5-related causes (ALRI, COPD, LC, IHD, STROKE).The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @importFrom magrittr %>%
#' @export



calc_daly_pm25<-function(){

  daly_calc_pm<-tibble::as_tibble(raw.daly) %>%
    dplyr::filter(rei == "Ambient particulate matter pollution") %>%
    dplyr::select(location_name = location, year,measure_name = measure, cause_name = cause, age,val) %>%
    dplyr::mutate(cause_name=dplyr::if_else(grepl("stroke", cause_name), "stroke", cause_name),
                  cause_name=dplyr::if_else(grepl("Lower respiratory", cause_name), "lri", cause_name),
                  cause_name=dplyr::if_else(grepl("lung cancer", cause_name), "lc", cause_name),
                  cause_name=dplyr::if_else(grepl("Ischemic heart disease", cause_name), "ihd", cause_name),
                  cause_name=dplyr::if_else(grepl("Chronic obstructive", cause_name), "copd", cause_name),
                  cause_name=dplyr::if_else(grepl("Diabetes", cause_name), "dm", cause_name)) %>%
    dplyr::mutate(age = dplyr::if_else(age == "All ages", ">25", age)) %>%
    tidyr::spread(measure_name, val) %>%
    dplyr::rename(country = location_name) %>%
    gcamdata::left_join_error_no_match(country_iso, by="country") %>%
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>%
                                         dplyr::rename(iso3 = subRegionAlt),
                                       by = "iso3") %>%
    dplyr::group_by(fasst_region, year, age, cause_name) %>%
    dplyr::summarise(`DALYs (Disability-Adjusted Life Years)` = sum(`DALYs (Disability-Adjusted Life Years)`),
                     Deaths = sum(Deaths)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(DALYs = `DALYs (Disability-Adjusted Life Years)`) %>%
    dplyr::mutate(DALY_ratio = DALYs / Deaths) %>%
    dplyr::select(fasst_region, year, age, cause_name, DALY_ratio) %>%
    dplyr::mutate(risk = "Ambient particulate matter pollution") %>%
    dplyr::rename(region = fasst_region,
                  disease = cause_name)


  invisible(daly_calc_pm)

}


#' calc_daly_o3
#'
#' Get the DALY-to-Mortality ratios used to estimate the Disability Adjusted Life Years (DALYs) attributable to ozone (O3) exposure.
#' @source Institute for Health Metrics and Evaluation (http://www.healthdata.org/)
#' @keywords DALYs, O3
#' @return DALY-to-Mortality ratios for TM5-FASST regions for all years and O3-related causes (respiratory disease).The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @importFrom magrittr %>%
#' @export


calc_daly_o3<-function(){

  daly_calc_o3<-tibble::as_tibble(raw.daly) %>%
    dplyr::filter(rei == "Ambient ozone pollution",
                  age == "All ages") %>%
    dplyr::select(-age) %>%
    dplyr::select(location_name = location, year,measure_name = measure, cause_name = cause, val) %>%
    dplyr::mutate(cause_name = dplyr::if_else(grepl("Chronic obstructive",cause_name), "copd", cause_name)) %>%
    tidyr::spread(measure_name, val) %>%
    dplyr::rename(country = location_name) %>%
    #dplyr::mutate(country = dplyr::if_else(country == "CÃ´te d'Ivoire", "Cote d'Ivoire", country)) %>%
    gcamdata::left_join_error_no_match(country_iso, by = "country") %>%
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>%
                                         dplyr::rename(iso3 = subRegionAlt),
                                       by = "iso3") %>%
    dplyr::group_by(fasst_region, year, cause_name) %>%
    dplyr::summarise(`DALYs (Disability-Adjusted Life Years)` = sum(`DALYs (Disability-Adjusted Life Years)`),
                     Deaths = sum(Deaths)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(DALYs = `DALYs (Disability-Adjusted Life Years)`) %>%
    dplyr::mutate(DALY_ratio = DALYs / Deaths) %>%
    dplyr::select(fasst_region, year, cause_name, DALY_ratio) %>%
    dplyr::mutate(risk = "Ambient ozone pollution") %>%
    dplyr::rename(region = fasst_region,
                  disease = cause_name)

  invisible(daly_calc_o3)

}


#' m3_get_mort_pm25
#'
#'
#' Produce premature mortality attributable to PM2.5 exposure based on the integrated exposure-response functions (IER) from Burnett et al (2014), consistent with the GBD 2016 study.
#' @keywords module_3, premature mortality, PM2.5
#' @source Burnett, R.T., Pope III, C.A., Ezzati, M., Olives, C., Lim, S.S., Mehta, S., Shin, H.H., Singh, G., Hubbell, B., Brauer, M. and Anderson, H.R., 2014. An integrated risk function for estimating the global burden of disease attributable to ambient fine particulate matter exposure. Environmental health perspectives, 122(4), pp.397-403.
#' @return Premature mortality attributable to PM2.5 exposure for each TM5-FASST regions for all years (# mortalities).
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param rdata_name Name of the RData file. It must contain the queries in a list
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_param Method to compute the premature mortality estimates used in the plotting: GBD, GEMM, or FUSION
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_mort_pm25<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name = NULL, prj = NULL,
                           rdata_name = NULL, scen_name, queries = "queries_rfasst.xml", final_db_year = 2100, mort_param = "GBD",
                           ssp = "SSP2", saveOutput = T, map = F, anim = T, recompute = F){

  if (!recompute & exists('m3_get_mort_pm25.output')) {
    return(m3_get_mort_pm25.output)
  } else {

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    all_years<-all_years[all_years <= final_db_year]

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_pm25_mort")) dir.create("output/maps/m3/maps_pm25_mort")

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

    # Get PM2.5
    pm.pre<-m2_get_conc_pm25(db_path, query_path, db_name, prj_name, prj, rdata_name, scen_name, queries,
                             saveOutput = F, final_db_year = final_db_year, recompute = recompute)
    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    rlang::inform('Computing premature deaths ...')


    # Get population with age groups
    pop.all.str<-get(paste0('pop.all.str.',ssp))
    # Get baseline mortality rates
    mort.rates<-calc_mort_rates() %>%
      dplyr::mutate(age = dplyr::if_else(age == "All Ages", ">25", age))


    # Get relative risk parameters
    GBD <- raw.rr.gbd.param
    GEMM <- raw.rr.gemm.param %>%
      rbind(c(">25", 0, 0, 0, 2.4, 0, "dm"))


    m3_get_mort_pm25.output <- list()
    for (sc in scen_name) {

      #------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------
      pm<- tibble::as_tibble(pm.pre %>%
                               dplyr::filter(scenario == sc)) %>%
        gcamdata::repeat_add_columns(tibble::tibble(disease = c('ihd','stroke'))) %>%
        gcamdata::repeat_add_columns(tibble::tibble(age = unique(raw.rr.gbd.param$age))) %>%
        dplyr::filter(year != ">25") %>%
        dplyr::bind_rows(tibble::as_tibble(pm.pre %>%
                                             dplyr::filter(scenario == sc)) %>%
                           gcamdata::repeat_add_columns(tibble::tibble(disease = c('copd','lc', "dm", "lri"))) %>%
                           dplyr::mutate(age = ">25"))

      pm.list.dis<-split(pm, pm$disease)

      calc_rr<-function(df){

        # colnames(df)<-c("region", "year", "units", "value", "disease", "age")

        df_fin <- df %>%
          dplyr::rowwise() %>%
          dplyr::left_join(GBD, by = c('disease', 'age')) %>%
          dplyr::filter(complete.cases(alpha)) %>%
          dplyr::mutate(GBD_rr = 1 + alpha * (1 - exp(-beta * max(0, value - zcf) ^ delta))) %>%
          dplyr::select(-alpha, -beta, -zcf, -delta) %>%
          gcamdata::left_join_error_no_match(GEMM, by = c('disease', 'age')) %>%
          dplyr::rename(nu = un) %>%
          dplyr::mutate(theta = as.numeric(theta),
                        alpha = as.numeric(alpha),
                        mu = as.numeric(mu),
                        nu = as.numeric(nu),
                        cf_pm = as.numeric(cf_pm)
          ) %>%
          dplyr::mutate(GEMM_rr =exp(theta * log(max(0, value - cf_pm)/ alpha + 1) / (1 + exp(-(max(0, value - cf_pm) - mu) / nu)))) %>%
          dplyr::select(-theta, -alpha, - mu, -nu, -cf_pm) %>%
          dplyr::rename(pm_conc = value)

        return(invisible(df_fin))

      }

      pm.rr.pre<-dplyr::bind_rows(lapply(pm.list.dis,calc_rr))

      # The FUSION model needs age-groups, so the calculation is slightly different
      pm.rr.fusion <- pm.rr.pre %>%
        dplyr::select(region, year, units, pm_conc, disease, age) %>%
        dplyr::mutate(z = round(pm_conc, 1)) %>%
        #dplyr::left_join(raw.rr.fusion, by = c('disease', 'age', 'z')) %>%
        gcamdata::left_join_error_no_match(raw.rr.fusion, by = c('disease', 'age', 'z')) %>%
        dplyr::select(region, year, units, pm_conc, disease, age, FUSION_rr = rr)

      pm.rr <- pm.rr.pre %>%
        gcamdata::left_join_error_no_match(pm.rr.fusion,
                                           by = c('region', 'year', 'units', 'pm_conc', 'disease', 'age'))


      #------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------
      # First, adjust population
      pop_fin_str <- pop.all.str %>%
        dplyr::mutate(pop_1K = value * 1E3,
                      unit = "1K",
                      year = as.numeric(year)) %>%
        dplyr::select(-scenario, -unit, -value)

      pop_fin_allages <- pop.all.str %>%
        dplyr::group_by(region, year) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(pop_1K = value * 1E3,
                      unit = "1K",
                      year = as.numeric(year)) %>%
        dplyr::select(-value)

      # Calculate premature mortalities
      pm.mort.pre<-tibble::as_tibble(pm.rr) %>%
        dplyr::filter(as.numeric(as.character(year)) >= 2010) %>%
        dplyr::select(-pm_conc) %>%
        tidyr::pivot_longer(cols = dplyr::ends_with("rr"),
                            names_to = "rr",
                            values_to = "value") %>%
        dplyr::arrange(disease) %>%
        tidyr::replace_na(list(value = 0))

      pm.mort.str <- pm.mort.pre %>%
        dplyr::filter(disease %in% c("ihd", "stroke")) %>%
        dplyr::mutate(year = as.numeric(as.character(year))) %>%
        gcamdata::left_join_error_no_match(mort.rates, by = c('region', 'year', 'disease', 'age')) %>%
        gcamdata::left_join_error_no_match(pop_fin_str, by = c('region', 'year', 'age')) %>%
        dplyr::mutate(mort = (1 - 1/ value) * rate * pop_1K / 100,
                      mort = round(mort, 0)) %>%
        dplyr::select(region, year, age, disease, pm_mort = mort, rr) %>%
        dplyr::mutate(rr = gsub("_rr", "", rr)) %>%
        tidyr::pivot_wider(names_from = rr,
                           values_from = pm_mort)

      pm.mort.allages <- pm.mort.pre %>%
        dplyr::filter(disease %!in% c("ihd", "stroke")) %>%
        dplyr::mutate(year = as.numeric(as.character(year))) %>%
        gcamdata::left_join_error_no_match(mort.rates, by = c('region', 'year', 'disease', 'age')) %>%
        gcamdata::left_join_error_no_match(pop_fin_allages, by = c('region', 'year')) %>%
        dplyr::mutate(mort = (1 - 1/ value) * rate * pop_1K / 100,
                      mort = round(mort, 0)) %>%
        dplyr::select(region, year, age, disease, pm_mort = mort, rr) %>%
        dplyr::mutate(rr = gsub("_rr", "", rr)) %>%
        # adjust missing value for dm in the GEMM model
        dplyr::mutate(pm_mort = dplyr::if_else(is.finite(pm_mort), pm_mort, 0)) %>%
        tidyr::pivot_wider(names_from = rr,
                           values_from = pm_mort)



      pm.mort <- dplyr::bind_rows(pm.mort.allages,
                                  pm.mort.str)


      #----------------------------------------------------------------------
      #----------------------------------------------------------------------
      # Write results

      pm.mort<-pm.mort %>%
        dplyr::mutate(scenario = sc)
      m3_get_mort_pm25.output <- append(m3_get_mort_pm25.output, list(pm.mort))

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Bind the results

    m3_get_mort_pm25.output <- dplyr::bind_rows(m3_get_mort_pm25.output)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If saveOutput=T,  writes aggregated PM2.5 values per TM5-FASST region

    pm.mort.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/", "m3/", "PM25_MORT_", paste(scen_name, collapse = "-"), "_", unique(df$year),".csv"), row.names = F)
    }

    pm.mort.agg.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/", "m3/", "PM25_MORT_AGG_", paste(scen_name, collapse = "-"), "_", unique(df$year),".csv"), row.names = F)
    }

    if(saveOutput == T){

      lapply(split(m3_get_mort_pm25.output, pm.mort$year),pm.mort.write)

      pm.mort.agg <- m3_get_mort_pm25.output %>%
        dplyr::group_by(region, year, scenario) %>%
        dplyr::summarise(FUSION = sum(FUSION),
                         GBD = sum(GBD),
                         GEMM = sum(GEMM)) %>%
        dplyr::ungroup()
      lapply(split(pm.mort.agg, pm.mort.agg$year),pm.mort.agg.write)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If map=T, it produces a map with the calculated outcomes

    if(map==T){

      pm.mort.agg <- m3_get_mort_pm25.output %>%
        dplyr::group_by(region, year, scenario) %>%
        dplyr::summarise(FUSION = sum(FUSION),
                         GBD = sum(GBD),
                         GEMM = sum(GEMM)) %>%
        dplyr::ungroup()

      pm.mort.map<-pm.mort.agg %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, all_of(mort_param), scenario) %>%
        dplyr::rename(value = all_of(mort_param)) %>%
        dplyr::mutate(units = "Mortalities",
                      year = as.numeric(as.character(year)))

      rmap::map(data = pm.mort.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_pm25_mort",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)
    }


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output

    return(invisible(m3_get_mort_pm25.output))

  }
}

#' m3_get_yll_pm25
#'
#'
#' Produce YLLs attributable to PM2.5 exposure. YLL-to-Mortalities ratios are based on TM5-FASST calculations. Premature mortalities are  based on the integrated exposure-response functions (IER) from Burnett et al (2014), consistent with the GBD 2016 study.
#' @keywords module_3, YLL, PM2.5
#' @return YLLs attributable to PM2.5 exposure for each TM5-FASST regions for all years (# YLLs). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param rdata_name Name of the RData file. It must contain the queries in a list
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_param Method to compute the premature mortality estimates used in the plotting: GBD, GEMM, or FUSION
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_yll_pm25<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name = NULL, prj = NULL,
                          rdata_name = NULL, scen_name, queries = "queries_rfasst.xml", final_db_year = 2100, mort_param = "GBD",
                          ssp = "SSP2", saveOutput = T, map = F, anim = T, recompute = F){

  if (!recompute & exists('m3_get_yll_pm25.output')) {
    return(m3_get_yll_pm25.output)
  } else {

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    all_years<-all_years[all_years <= final_db_year]

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_pm25_yll")) dir.create("output/maps/m3/maps_pm25_yll")

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

    # Get pm.mort
    pm.mort<-m3_get_mort_pm25(db_path = db_path, db_name = db_name, prj_name = prj_name, prj = prj, scen_name = scen_name, rdata_name = rdata_name, query_path = query_path,
                              queries = queries, ssp = ssp, saveOutput = F, final_db_year = final_db_year, recompute = recompute)



    m3_get_yll_pm25.output <- list()
    for (sc in scen_name) {

      # Get years of life lost
      yll.pm.mort<-tibble::as_tibble(raw.yll.pm25) %>%
        dplyr::filter(year == max(as.numeric(year))) %>%
        dplyr::select(-year) %>%
        dplyr::select(location_name = location, measure_name = measure, cause_name = cause, age, val) %>%
        dplyr::mutate(cause_name=dplyr::if_else(grepl("stroke", cause_name), "stroke", cause_name),
                      cause_name=dplyr::if_else(grepl("Lower respiratory", cause_name), "lri", cause_name),
                      cause_name=dplyr::if_else(grepl("lung cancer", cause_name), "lc", cause_name),
                      cause_name=dplyr::if_else(grepl("Ischemic heart disease", cause_name), "ihd", cause_name),
                      cause_name=dplyr::if_else(grepl("Chronic obstructive", cause_name), "copd", cause_name),
                      cause_name=dplyr::if_else(grepl("Diabetes", cause_name), "dm", cause_name)) %>%
        dplyr::mutate(age = dplyr::if_else(age == "All ages", ">25", age)) %>%
        tidyr::spread(measure_name, val) %>%
        dplyr::rename(country = location_name) %>%
        #dplyr::mutate(country = dplyr::if_else(country == "CÃ´te d'Ivoire", "Cote d'Ivoire", country)) %>%
        gcamdata::left_join_error_no_match(country_iso, by="country") %>%
        gcamdata::left_join_error_no_match(rfasst::fasst_reg %>%
                                             dplyr::rename(iso3 = subRegionAlt),
                                           by = "iso3") %>%
        dplyr::group_by(fasst_region, age, cause_name) %>%
        dplyr::summarise(`YLLs (Years of Life Lost)` = sum(`YLLs (Years of Life Lost)`),
                         Deaths = sum(Deaths)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(YLLs = `YLLs (Years of Life Lost)`) %>%
        dplyr::mutate(YLL_ratio = YLLs / Deaths) %>%
        dplyr::select(fasst_region, age, cause_name, YLL_ratio) %>%
        dplyr::mutate(risk = "Ambient particulate matter pollution") %>%
        dplyr::rename(region = fasst_region,
                      disease = cause_name) %>%
        gcamdata::repeat_add_columns(tibble::tibble(year = unique(pm.mort %>%
                                                                    dplyr::filter(scenario == sc) %>%
                                                                    dplyr::pull(year)))) %>%
        dplyr::mutate(age = gsub(" years", "", age))

      yll.pm.mort <- dplyr::bind_rows(
        yll.pm.mort,
        yll.pm.mort %>% dplyr::filter(region == "RUS") %>% dplyr::mutate(region = "RUE")
      )



      #------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------
      pm.yll<- tibble::as_tibble(pm.mort %>%
                                   dplyr::filter(scenario == sc)) %>%
        dplyr::filter(disease != "tot") %>%
        gcamdata::left_join_error_no_match(yll.pm.mort, by = c("region", "disease", "year", "age")) %>%
        dplyr::mutate(yll_GBD = GBD * YLL_ratio,
                      yll_GEMM = GEMM * YLL_ratio,
                      yll_FUSION = FUSION * YLL_ratio) %>%
        dplyr::select(region, year, disease, yll_GBD, yll_GEMM, yll_FUSION)

      #------------------------------------------------------------------------------------
      # Write the output

      pm.yll.fin<-pm.yll %>%
        dplyr::mutate(scenario = sc)
      m3_get_yll_pm25.output <- append(m3_get_yll_pm25.output, list(pm.yll.fin))

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Bind the results

    m3_get_yll_pm25.output <<- dplyr::bind_rows(m3_get_yll_pm25.output)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If saveOutput=T,  writes aggregated PM2.5 values per TM5-FASST region

    pm.yll.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","PM25_YLL_", paste(scen_name, collapse = "-"), "_", unique(df$year),".csv"), row.names = F)
    }

    pm.yll.tot.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","PM25_YLL_TOT_", paste(scen_name, collapse = "-"), "_", unique(df$year),".csv"), row.names = F)
    }

    if(saveOutput == T){

      lapply(split(m3_get_yll_pm25.output, pm.mort$year),pm.yll.write)

      pm.yll.tot <- m3_get_yll_pm25.output %>%
        dplyr::group_by(region, year, scenario) %>%
        dplyr::summarise(yll_GBD = sum(yll_GBD),
                         yll_GEMM = sum(yll_GBD),
                         yll_FUSION = sum(yll_FUSION)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(disease = "tot")
      lapply(split(pm.yll.tot, pm.yll.tot$year),pm.yll.tot.write)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If map=T, it produces a map with the calculated outcomes

    if(map == T){

      pm.yll.tot <- m3_get_yll_pm25.output %>%
        dplyr::group_by(region, year, scenario) %>%
        dplyr::summarise(yll_GBD = sum(yll_GBD),
                         yll_GEMM = sum(yll_GBD),
                         yll_FUSION = sum(yll_FUSION)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(disease = "tot")

      pm.yll.fin.map<-pm.yll.tot %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, disease, all_of(paste0('yll_',mort_param)), scenario) %>%
        dplyr::rename(value = all_of(paste0('yll_',mort_param)),
                      class = disease) %>%
        dplyr::mutate(year = as.numeric(as.character(year)),
                      units = "YLLs")

      rmap::map(data = pm.yll.fin.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_pm25_yll",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output

    return(invisible(m3_get_yll_pm25.output))
  }

}



#' m3_get_daly_pm25
#'
#'
#' Produce Disability Adjusted Life Years (DALYs) attributable to PM2.5 exposure. See calc_daly_pm for detials on DALY-to-Mortality ratios.
#' @source Institute for Health Metrics and Evaluation (http://www.healthdata.org/)
#' @keywords module_3, DALY, PM2.5,
#' @return Disability Adjusted Life Years (DALYs) attributable to PM2.5 exposure for each TM5-FASST regions for all years (# DALYs). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param rdata_name Name of the RData file. It must contain the queries in a list
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_param Method to compute the premature mortality estimates used in the plotting: GBD, GEMM, or FUSION
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_daly_pm25<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name = NULL, prj = NULL,
                           rdata_name = NULL, scen_name, queries = "queries_rfasst.xml", final_db_year = 2100, mort_param = "GBD",
                           ssp="SSP2", saveOutput = T, map = F, anim = T, recompute = F){


  if (!recompute & exists('m3_get_daly_pm25.output')) {
    return(m3_get_daly_pm25.output)
  } else {

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    all_years<-all_years[all_years <= final_db_year]

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_pm25_daly")) dir.create("output/maps/m3/maps_pm25_daly")

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

    # Get DALYs
    daly_calc_pm<-calc_daly_pm25()

    # Get pm.mort
    pm.mort<-m3_get_mort_pm25(db_path = db_path, db_name = db_name, prj_name = prj_name, prj = prj, scen_name = scen_name, rdata_name = rdata_name, query_path = query_path,
                              queries = queries, ssp = ssp, saveOutput = F, final_db_year = final_db_year, recompute = recompute)


    m3_get_daly_pm25.output <- list()
    for (sc in scen_name) {

      #------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------
      daly.calc.pm.adj<-tibble::as_tibble(daly_calc_pm) %>%
        dplyr::filter(year == max(year)) %>%
        dplyr::select(-year) %>%
        gcamdata::repeat_add_columns(tibble::tibble(year = unique(levels(as.factor(pm.mort %>%
                                                                                     dplyr::filter(scenario == sc) %>%
                                                                                     dplyr::pull(year)))))) %>%
        dplyr::bind_rows(tibble::as_tibble(daly_calc_pm) %>%
                           dplyr::filter(year == max(year),
                                         region == "RUS") %>%
                           dplyr::select(-year) %>%
                           dplyr::mutate(region = "RUE") %>%
                    gcamdata::repeat_add_columns(tibble::tibble(year = unique(levels(as.factor(pm.mort$year)))))) %>%
        dplyr::mutate(year = as.numeric(year))


      pm.daly<- tibble::as_tibble(pm.mort %>%
                                    dplyr::filter(scenario == sc)) %>%
        dplyr::filter(disease != "tot") %>%
        gcamdata::left_join_error_no_match(daly.calc.pm.adj, by = c("region","disease","year", "age")) %>%
        dplyr::mutate(daly_GBD = GBD * DALY_ratio,
                      daly_GEMM = GEMM * DALY_ratio,
                      daly_FUSION = FUSION * DALY_ratio) %>%
        dplyr::select(region, year, disease, age, daly_GBD, daly_GEMM, daly_FUSION)


      #------------------------------------------------------------------------------------
      # Write the output

      pm.daly.list<-pm.daly %>%
        dplyr::mutate(scenario = sc)
      m3_get_daly_pm25.output <- append(m3_get_daly_pm25.output, list(pm.daly.list))

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Bind the results

    m3_get_daly_pm25.output <<- dplyr::bind_rows(m3_get_daly_pm25.output)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If saveOutput=T,  writes aggregated PM2.5 values per TM5-FASST region

    pm.daly.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","PM25_DALY_",paste(scen_name, collapse = "-"),"_",unique(df$year),".csv"),row.names = F)
    }

    pm.daly.tot.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","PM25_DALY_TOT_",paste(scen_name, collapse = "-"),"_",unique(df$year),".csv"),row.names = F)
    }

    if(saveOutput==T){

      lapply(split(m3_get_daly_pm25.output, pm.daly$year),pm.daly.write)

      pm.daly.tot <- m3_get_daly_pm25.output %>%
        dplyr::group_by(region, year, scenario) %>%
        dplyr::summarise(daly_GBD = sum(daly_GBD),
                         daly_GEMM = sum(daly_GEMM),
                         daly_FUSION = sum(daly_FUSION)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(disease = "tot")
      lapply(split(pm.daly.tot, pm.daly.tot$year),pm.daly.tot.write)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If map=T, it produces a map with the calculated outcomes

    if(map == T){

      pm.daly.tot <- m3_get_daly_pm25.output %>%
        dplyr::group_by(region, year, scenario) %>%
        dplyr::summarise(daly_GBD = sum(daly_GBD),
                         daly_GEMM = sum(daly_GEMM),
                         daly_FUSION = sum(daly_FUSION)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(disease = "tot")

      pm.daly.tot.fin.map<-pm.daly.tot %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, disease, all_of(paste0('daly_',mort_param)), scenario) %>%
        dplyr::rename(value = all_of(paste0('daly_',mort_param)),
                      class = disease) %>%
        dplyr::mutate(year = as.numeric(as.character(year)),
                      units = "DALYs")

      rmap::map(data = pm.daly.tot.fin.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_pm25_daly",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output

    return(invisible(m3_get_daly_pm25.output))
  }

}



#' m3_get_mort_o3
#'
#'
#' Produce premature mortality attributable to O3 exposure (measured by the M6M indicator) based on the integrated exposure-response functions (IER) from Jerret et al (2009), consistent with the GBD 2016 study.
#' @keywords module_3, premature mortality, O3
#' @source Jerrett, M., Burnett, R.T., Pope III, C.A., Ito, K., Thurston, G., Krewski, D., Shi, Y., Calle, E. and Thun, M., 2009. Long-term ozone exposure and mortality. New England Journal of Medicine, 360(11), pp.1085-1095.
#' @return Premature mortality attributable to O3 exposure for  TM5-FASST regions for all years (# mortalties). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param rdata_name Name of the RData file. It must contain the queries in a list
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_param Method to compute the premature mortality estimates used in the plotting: Jerret2009, GBD2016
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_mort_o3<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name = NULL, prj = NULL,
                         rdata_name = NULL, scen_name, queries = "queries_rfasst.xml", final_db_year = 2100, mort_param = "Jerret2009",
                         ssp = "SSP2", saveOutput = T, map = F, anim = T, recompute = F){

  if (!recompute & exists('m3_get_mort_o3.output')) {
    return(m3_get_mort_o3.output)
  } else {

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    all_years<-all_years[all_years <= final_db_year]

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_o3_mort")) dir.create("output/maps/m3/maps_o3_mort")

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

    # Get M6M
    m6m <- m2_get_conc_m6m(db_path, query_path, db_name, prj_name, prj, rdata_name = rdata_name, scen_name, queries, saveOutput = F, final_db_year = final_db_year, recompute = recompute)

    # Get population
    pop.all<-get(paste0('pop.all.',ssp))
    # Get baseline mortality rates
    mort.rates.o3<-calc_mort_rates() %>%
      dplyr::ungroup() %>%
      dplyr::filter(disease == "copd") %>%
      dplyr::select(-age) %>%
      dplyr::distinct()


    m3_get_mort_o3.output <- list()
    for (sc in scen_name) {

      #------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------
      # Premature mortality
      o3.mort<-tibble::as_tibble(m6m %>%
                                   dplyr::filter(scenario == sc)) %>%
        dplyr::mutate(year = as.numeric(as.character(year))) %>%
        dplyr::filter(year >= 2010) %>%
        gcamdata::repeat_add_columns(tibble::tibble(disease = "copd")) %>%
        dplyr::select(-units) %>%
        dplyr::rename(m6m = value) %>%
        dplyr::mutate(year = as.character(year)) %>%
        gcamdata::left_join_error_no_match(pop.all, by = c("region","year")) %>%
        dplyr::mutate(pop_af = pop_tot * 1E6 ,
                      year = as.numeric(year)) %>%
        gcamdata::left_join_error_no_match(mort.rates.o3 %>%
                                             dplyr::filter(year >= 2010) %>%
                                             dplyr::rename(mr_resp = rate),
                                           by=c("region", "year", "disease")) %>%
        dplyr::mutate(adj_jer_med = 1 - exp(-(m6m - cf_o3) * rr_resp_o3_Jerret2009_med / 100000),
                      adj_jer_med = dplyr::if_else(adj_jer_med < 0, 0, adj_jer_med),
                      mort_o3_jer_med = round(pop_af * mr_resp * adj_jer_med, 0),

                      adj_gdb2016_med = 1 - exp(-(m6m - cf_o3) * rr_resp_o3_GBD2016_med / 100000),
                      adj_gdb2016_med = dplyr::if_else(adj_gdb2016_med < 0, 0, adj_gdb2016_med),
                      mort_o3_gbd2016_med = round(pop_af * mr_resp * adj_gdb2016_med, 0)) %>%
        dplyr::select(region, year, disease, Jerret2009 = mort_o3_jer_med,  GBD2016 =  mort_o3_gbd2016_med)

      #------------------------------------------------------------------------------------
      # Write the output

      o3.mort.list<-o3.mort %>%
        dplyr::mutate(scenario = sc)
      m3_get_mort_o3.output <- append(m3_get_mort_o3.output, list(o3.mort.list))
    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Bind the results

    m3_get_mort_o3.output <<- dplyr::bind_rows(m3_get_mort_o3.output)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If saveOutput=T,  writes aggregated PM2.5 values per TM5-FASST region

    o3.mort.list<-split(m3_get_mort_o3.output,o3.mort$year)

    o3.mort.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","O3_MORT_",paste(scen_name, collapse = "-"),"_",unique(df$year),".csv"),row.names = F)
    }

    if(saveOutput == T) lapply(o3.mort.list,o3.mort.write)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If map=T, it produces a map with the calculated outcomes

    if(map == T){
      o3.mort.map<-m3_get_mort_o3.output %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, all_of(mort_param), scenario) %>%
        dplyr::rename(value = all_of(mort_param)) %>%
        dplyr::mutate(year = as.numeric(as.character(year)),
                      units = "Mortalities")

      rmap::map(data = o3.mort.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_o3_mort",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)

    }
    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output

    return(invisible(m3_get_mort_o3.output))
  }

}


#' m3_get_yll_o3
#'
#'
#' Produce YLLs attributable to O3 (M6M) exposure. YLL-to-Mortalities ratios are based on TM5-FASST calculations. Premature mortalities are based on the IER functions from Jerret et al (2009), consistent with the GBD 2016 study.
#' @keywords module_3, YLL, O3
#' @return YLLs attributable to O3 exposure for each TM5-FASST regions for all years (# YLLs). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param rdata_name Name of the RData file. It must contain the queries in a list
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_param Select the health function (GBD 2016 or Jerret et al 2009) and the Low/Med/High RR. By default = mort_o3_gbd2016_med
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_yll_o3<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name = NULL, prj = NULL,
                        rdata_name = NULL, scen_name, queries = "queries_rfasst.xml", final_db_year = 2100, mort_param = "Jerret2009",
                        ssp = "SSP2", saveOutput = T, map = F, anim = T, recompute = F){

  if (!recompute & exists('m3_get_yll_o3.output')) {
    return(m3_get_yll_o3.output)
  } else {
    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    all_years<-all_years[all_years <= final_db_year]

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_o3_yll")) dir.create("output/maps/m3/maps_o3_yll")

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

    # Get pm.mort
    o3.mort<-m3_get_mort_o3(db_path = db_path, db_name = db_name, prj_name = prj_name, prj = prj, scen_name = scen_name, rdata_name = rdata_name, query_path = query_path,
                            queries = queries, ssp = ssp, saveOutput = F, final_db_year = final_db_year, recompute = recompute)

    m3_get_yll_o3.output <- list()
    for (sc in scen_name) {

      #------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------
      o3.yll <- tibble::as_tibble(raw.yll.o3) %>%
        dplyr::filter(year == max(as.numeric(year))) %>%
        dplyr::select(-year) %>%
        dplyr::filter(age == "All ages") %>%
        dplyr::mutate(cause = dplyr::if_else(grepl("Chronic obstructive", cause), "copd", cause)) %>%
        dplyr::filter(cause == "copd",
                      grepl("ozone", rei)) %>%
        dplyr::select(location_name = location, measure_name = measure, cause_name = cause, age, val) %>%
        tidyr::spread(measure_name, val) %>%
        dplyr::rename(country = location_name) %>%
        #dplyr::mutate(country = dplyr::if_else(country == "CÃ´te d'Ivoire", "Cote d'Ivoire", country)) %>%
        gcamdata::left_join_error_no_match(country_iso, by="country") %>%
        gcamdata::left_join_error_no_match(rfasst::fasst_reg %>%
                                             dplyr::rename(iso3 = subRegionAlt),
                                           by = "iso3") %>%
        dplyr::group_by(fasst_region, age, cause_name) %>%
        dplyr::summarise(`YLLs (Years of Life Lost)` = sum(`YLLs (Years of Life Lost)`),
                         Deaths = sum(Deaths)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(YLLs = `YLLs (Years of Life Lost)`) %>%
        dplyr::mutate(YLL_ratio = YLLs / Deaths) %>%
        dplyr::select(fasst_region, cause_name, YLL_ratio) %>%
        dplyr::mutate(risk = "Ambient ozone pollution") %>%
        dplyr::rename(region = fasst_region,
                      disease = cause_name) %>%
        gcamdata::repeat_add_columns(tibble::tibble(year = unique(o3.mort %>%
                                                                    dplyr::filter(scenario == sc) %>%
                                                                    dplyr::pull(year))))

      o3.yll <- dplyr::bind_rows(
        o3.yll,
        o3.yll %>% dplyr::filter(region == "RUS") %>% dplyr::mutate(region = "RUE")
      )


      o3.yll.fin<- tibble::as_tibble(o3.mort %>%
                                       dplyr::filter(scenario == sc)) %>%
        dplyr::filter(disease != "tot") %>%
        gcamdata::left_join_error_no_match(o3.yll, by = c("region", "disease", "year")) %>%
        dplyr::mutate(yll_Jerret2009 = Jerret2009 * YLL_ratio) %>%
        dplyr::mutate(yll_GBD2016 = GBD2016 * YLL_ratio) %>%
        dplyr::select(region, year, disease, yll_Jerret2009, yll_GBD2016)


      #------------------------------------------------------------------------------------
      # Write the output

      o3.yll.fin.list<-o3.yll.fin %>%
        dplyr::mutate(scenario = sc)
      m3_get_yll_o3.output <- append(m3_get_yll_o3.output, list(o3.yll.fin.list))
    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Bind the results

    m3_get_yll_o3.output <<- dplyr::bind_rows(m3_get_yll_o3.output)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If saveOutput=T,  writes aggregated PM2.5 values per TM5-FASST region

    o3.yll.list<-split(m3_get_yll_o3.output, o3.yll.fin$year)

    o3.yll.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","O3_YLL_",paste(scen_name, collapse = "-"),"_",unique(df$year),".csv"),row.names = F)
    }


    if(saveOutput == T) lapply(o3.yll.list,o3.yll.write)

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If map=T, it produces a map with the calculated outcomes

    if(map == T){
      o3.yll.map <- m3_get_yll_o3.output %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, all_of(paste0('yll_',mort_param)), scenario) %>%
        dplyr::rename(value = paste0('yll_',mort_param)) %>%
        dplyr::mutate(year = as.numeric(as.character(year)),
                      units = "YLLs")

      rmap::map(data = o3.yll.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_o3_yll",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)

    }
    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output

    return(invisible(m3_get_yll_o3.output))
  }

}



#' m3_get_daly_o3
#'
#'
#' Produce Disability Adjusted Life Years (DALYs) attributable to O3 (M6M) exposure. See calc_daly_o3 for detials on DALY-to-Mortality ratios.
#' @source Institute for Health Metrics and Evaluation (http://www.healthdata.org/)
#' @keywords module_3, DALY, O3,
#' @return Disability Adjusted Life Years (DALYs) attributable to O3 exposure for each TM5-FASST regions for all years (# DALYs). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param rdata_name Name of the RData file. It must contain the queries in a list
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_param Method to compute the premature mortality estimates used in the plotting: Jerret2009, GBD2016
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_daly_o3<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name = NULL, prj = NULL,
                         rdata_name = NULL, scen_name, queries = "queries_rfasst.xml", final_db_year = 2100, mort_param = "Jerret2009",
                         ssp = "SSP2", saveOutput = T, map = F, anim = T, recompute = F){

  if (!recompute & exists('m3_get_daly_o3.output')) {
    return(m3_get_daly_o3.output)
  } else {

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    all_years<-all_years[all_years <= final_db_year]

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_o3_daly")) dir.create("output/maps/m3/maps_o3_daly")

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

    # Get DALYs
    daly_calc_o3<-calc_daly_o3()

    # Get pm.mort
    o3.mort <- m3_get_mort_o3(db_path = db_path, db_name = db_name, prj_name = prj_name, prj = prj, scen_name = scen_name, rdata_name = rdata_name, query_path = query_path,
                            queries = queries, ssp = ssp, saveOutput = F, final_db_year = final_db_year, recompute = recompute)

    m3_get_daly_o3.output <- list()
    for (sc in scen_name) {

      #------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------
      daly.calc.o3.adj<-tibble::as_tibble(daly_calc_o3) %>%
        dplyr::filter(year == max(year)) %>%
        dplyr::select(-year) %>%
        gcamdata::repeat_add_columns(tibble::tibble(year = unique(levels(as.factor(o3.mort %>%
                                                                                     dplyr::filter(scenario == sc) %>%
                                                                                     dplyr::pull(year)))))) %>%
        dplyr::bind_rows(tibble::as_tibble(daly_calc_o3) %>%
                           dplyr::filter(year == max(year),
                                         region == "RUS") %>%
                           dplyr::select(-year) %>%
                           dplyr::mutate(region = "RUE") %>%
                    gcamdata::repeat_add_columns(tibble::tibble(year = unique(levels(as.factor(o3.mort %>%
                                                                                                 dplyr::filter(scenario == sc) %>%
                                                                                                 dplyr::pull(year))))))) %>%
        dplyr::mutate(disease = "copd",
                      year = as.numeric(year))


      o3.daly<- tibble::as_tibble(o3.mort %>%
                                    dplyr::filter(scenario == sc)) %>%
        dplyr::filter(disease != "tot") %>%
        gcamdata::left_join_error_no_match(daly.calc.o3.adj, by = c("region","disease","year")) %>%
        dplyr::mutate(daly_Jerret2009 = round(Jerret2009 * DALY_ratio, 0),
                      daly_GBD2016 = round(GBD2016 * DALY_ratio, 0)) %>%
        dplyr::select(region, year, disease, daly_Jerret2009, daly_GBD2016 )


      #------------------------------------------------------------------------------------
      # Write the output

      o3.daly.list<-o3.daly %>%
        dplyr::mutate(scenario = sc)
      m3_get_daly_o3.output <- append(m3_get_daly_o3.output, list(o3.daly.list))
    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Bind the results

    m3_get_daly_o3.output <<- dplyr::bind_rows(m3_get_daly_o3.output)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If saveOutput=T,  writes aggregated PM2.5 values per TM5-FASST region

    o3.daly.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","O3_DALY_",paste(scen_name, collapse = "-"),"_",unique(df$year),".csv"), row.names = F)
    }

    o3.daly.tot.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","O3_DALY_TOT_",paste(scen_name, collapse = "-"),"_",unique(df$year),".csv"), row.names = F)
    }


    if(saveOutput == T){

      o3.daly.list<-split(m3_get_daly_o3.output,m3_get_daly_o3.output$year)

      o3.daly.tot<-m3_get_daly_o3.output %>%
        dplyr::group_by(region, year, scenario) %>%
        dplyr::summarise(daly_Jerret2009 = sum(daly_Jerret2009),
                         daly_GBD2016 = sum(daly_GBD2016)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(disease = "tot")
      o3.daly.tot.list<-split(o3.daly.tot,o3.daly.tot$year)

      lapply(o3.daly.list, o3.daly.write)
      lapply(o3.daly.tot.list, o3.daly.tot.write)
    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If map=T, it produces a map with the calculated outcomes

    if(map == T){

      o3.daly.tot<-m3_get_daly_o3.output %>%
        dplyr::group_by(region, year, scenario) %>%
        dplyr::summarise(daly_Jerret2009 = sum(daly_Jerret2009),
                         daly_GBD2016 = sum(daly_GBD2016)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(disease = "tot")

      o3.daly.tot.fin.map<-o3.daly.tot %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE",
                      disease == "tot") %>%
        dplyr::select(subRegion, year, all_of(paste0('daly_',mort_param)), scenario) %>%
        dplyr::rename(value = paste0('daly_',mort_param)) %>%
        dplyr::mutate(year = as.numeric(as.character(year)),
                      units = "DALYs")

      rmap::map(data = o3.daly.tot.fin.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_o3_daly",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)
    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output

    return(invisible(m3_get_daly_o3.output))
  }

}




