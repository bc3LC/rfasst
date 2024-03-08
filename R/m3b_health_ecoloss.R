#' m3_get_mort_pm25_ecoloss
#'
#'
#' Produce economic damages associated with premature mortality attributable to PM2.5 exposure based on the IER functions from Burnett et al (2014), consistent with the GBD 2016 study. The economic valuation takes as a base value the widely accepted Value of Statistical Life (VSL) of the OECD for 2005. This value, according to the literature ranges between US$1.8 and $4.5 million. The calculations for all regions are based on the  “unit value transfer approach” which adjusts the VSL according to their GDP and GDP growth rates. (Markandya et al 2018)
#' @source Narain, U. and Sall, C., 2016. Methodology for Valuing the Health Impacts of Air Pollution//// Markandya, A., Sampedro, J., Smith, S.J., Van Dingenen, R., Pizarro-Irizar, C., Arto, I. and González-Eguino, M., 2018. Health co-benefits from air pollution and mitigation costs of the Paris Agreement: a modelling study. The Lancet Planetary Health, 2(3), pp.e126-e133.
#' @keywords module_3, VSL ,premature mortality, PM2.5
#' @return Economic damages associated with mortality attributable to PM2.5 exposure for each TM5-FASST regions for all years (Million$2015). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param rdata_name Name of the RData file. It must contain the queries in a list
#' @param scen_name Name of the GCAM scenario to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_param Select the health function (GBD 2016, Burnett et al, 2014 (IERs), or Burnett et al 2018 (GEMM)) and the Low/Med/High RR. By default = GBD2016_medium
#' @param Damage_vsl_range Select the VSL to calculate the damages (Damage_vsl_med, Damage_vsl_low, or Damage_vsl_high)
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_mort_pm25_ecoloss<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name,
                                   rdata_name = NULL, scen_name, ssp = "SSP2", final_db_year = 2100,
                                   mort_param = "GBD2016_medium",  Damage_vsl_range = "Damage_vsl_med",
                                   queries = "queries_rfasst.xml", saveOutput = T, map = F, anim = T, recompute = F){

  if (!recompute & exists('m3_get_mort_pm25_ecoloss.output')) {
    return(m3_get_mort_pm25_ecoloss.output)
  } else {

    all_years<-all_years[all_years <= final_db_year]

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

    fasstSubset<-fasstSubset %>%
      dplyr::mutate(subRegionAlt=as.character(subRegionAlt)) %>%
      dplyr::left_join(fasst_reg, by = "subRegionAlt") %>%
      dplyr::select(-subRegion) %>%
      dplyr::rename(subRegion = fasst_region) %>%
      dplyr::mutate(subRegionAlt = as.factor(subRegionAlt))

    # Get Mortalities
    pm.mort<-m3_get_mort_pm25(db_path, query_path, db_name, prj_name, scen_name, queries, ssp = ssp, saveOutput = F, final_db_year = final_db_year, recompute = recompute) %>%
      dplyr::select(region, year, disease, mort_param) %>%
      dplyr::rename(mort_pm25 = mort_param)

    # Get gdp_pc
    gdp_pc<-get(paste0('gdp_pc.',ssp))

    #------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------
    # Economic valuation of premature mortality: Value of Statistical Life (VSL)
    vsl<-gdp_pc %>%
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
      dplyr::mutate(unit = "Million$2005")

    #------------------------------------------------------------------------------------
    pm.mort.EcoLoss<-pm.mort %>%
      gcamdata::left_join_error_no_match(vsl, by=c("region", "year")) %>%
      dplyr::select(-scenario) %>%
      # Calculate the median damages
      dplyr::mutate(Damage_vsl_med = round(mort_pm25 * vsl_med * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                    Damage_vsl_low = round(mort_pm25 * vsl_lb * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                    Damage_vsl_high = round(mort_pm25 * vsl_ub * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                    unit = "Million$2015") %>%
      dplyr::select(region, year, disease, Damage_vsl_range, unit)


    #------------------------------------------------------------------------------------
    # Write the output
    pm.mort.EcoLoss.list<-split(pm.mort.EcoLoss,pm.mort.EcoLoss$year)


    pm.mort.EcoLoss.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","PM25_MORT_ECOLOSS_",scen_name[1],"_",unique(df$year),".csv"),row.names = F)
    }


    if(saveOutput == T){

      lapply(pm.mort.EcoLoss.list, pm.mort.EcoLoss.write)

    }
    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    # If map=T, it produces a map with the calculated outcomes

    if(map == T){
      pm.mort.EcoLoss.map<-pm.mort.EcoLoss %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, disease, Damage_vsl_range, unit) %>%
        dplyr::rename(value = Damage_vsl_range,
                      class = disease,
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

    }
    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    pm.mort.EcoLoss<-dplyr::bind_rows(pm.mort.EcoLoss.list)
    m3_get_mort_pm25_ecoloss.output <<- pm.mort.EcoLoss
    return(invisible(pm.mort.EcoLoss))
  }

}


#' m3_get_yll_pm25_ecoloss
#'
#'
#' Produce Economic damages associated with YLLs attributable to PM2.5.The economic valuation takes as a base value the Value of Statistical Life Year (VSLY) for EU from Schlander et al (2017) and expands the value to other regions based on the“unit value transfer approach” which adjusts the VSLY according to their GDP and GDP growth rates. . .YLL-to-Mortalities ratios are based on TM5-FASST calculations. Premature mortalities are  based on the integrated exposure-response functions (IER) from Burnett et al (2014), consistent with the GBD 2016 study.
#' @keywords module_3, YLL, PM2.5, VSLY
#' @source Schlander, M., Schaefer, R. and Schwarz, O., 2017. Empirical studies on the economic value of a Statistical Life Year (VSLY) in Europe: what do they tell us?. Value in Health, 20(9), p.A666.
#' @return Economic damages associated with YLLs attributable to PM2.5 exposure for each TM5-FASST regions for all years (Thous$2015).The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param rdata_name Name of the RData file. It must contain the queries in a list
#' @param scen_name Name of the GCAM scenario to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_param Select the health function (GBD 2016, Burnett et al, 2014 (IERs), or Burnett et al 2018 (GEMM)) and the Low/Med/High RR. By default = GBD2016_medium
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_yll_pm25_ecoloss<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name,
                                  rdata_name = NULL, scen_name, queries = "queries_rfasst.xml", final_db_year = 2100, mort_param = "GBD2016_medium",
                                  ssp = "SSP2", saveOutput = T, map = F, anim = T, recompute = F){

  if (!recompute & exists('m3_get_yll_pm25_ecoloss.output')) {
    return(m3_get_yll_pm25_ecoloss.output)
  } else {

    all_years<-all_years[all_years <= final_db_year]

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_pm25_yll_ecoloss")) dir.create("output/maps/m3/maps_pm25_yll_ecoloss")

    # Ancillary Functions
    `%!in%` = Negate(`%in%`)

    # Shape subset for maps
    fasstSubset <- rmap::mapCountries

    fasstSubset<-fasstSubset %>%
      dplyr::mutate(subRegionAlt = as.character(subRegionAlt)) %>%
      dplyr::left_join(fasst_reg, by = "subRegionAlt") %>%
      dplyr::select(-subRegion) %>%
      dplyr::rename(subRegion = fasst_region) %>%
      dplyr::mutate(subRegionAlt = as.factor(subRegionAlt))

    # Get Mortalities
    pm.yll.fin<-m3_get_yll_pm25(db_path, query_path, db_name, prj_name, scen_name, queries, ssp = ssp, saveOutput = F,
                                final_db_year = final_db_year, mort_param = mort_param, recompute = recompute)

    # Get gdp_pc
    gdp_pc<-get(paste0('gdp_pc.',ssp))

    #------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------
    # Economic valuation years of life lost
    vsly<-gdp_pc %>%
      # calculate the adjustment factor
      dplyr::mutate(adj = (gdp_pc / gdp_eu_2005) ^ inc_elas_vsl) %>%
      # multiply the LB and the UB of the European VSL (in 2005$), and take the median value
      dplyr::mutate(vsly = adj * vsly_eu_2005) %>%
      dplyr::select(scenario, region, year, vsly) %>%
      dplyr::mutate(vsly = vsly * 1E-3) %>%
      dplyr::mutate(unit = "Thous$2005")
    #------------------------------------------------------------------------------------

    pm.yll.EcoLoss<-pm.yll.fin %>%
      gcamdata::left_join_error_no_match(vsly, by = c("region","year")) %>%
      dplyr::select(-scenario) %>%
      dplyr::mutate(Damage_med = round(yll * vsly * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
             unit="Thous$2015")

    #------------------------------------------------------------------------------------
    # Write the output
    pm.yll.EcoLoss.list<-split(pm.yll.EcoLoss,pm.yll.EcoLoss$year)


    pm.yll.EcoLoss.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","PM25_YLL_ECOLOSS_",scen_name[1],"_",unique(df$year),".csv"),row.names = F)
    }


    if(saveOutput==T){

      lapply(pm.yll.EcoLoss.list, pm.yll.EcoLoss.write)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    # If map=T, it produces a map with the calculated outcomes

    if(map == T){
      pm.yll.EcoLoss.map<-pm.yll.EcoLoss %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, disease, Damage_med, unit) %>%
        dplyr::rename(value = Damage_med,
                      class = disease,
                      units = unit) %>%
        dplyr::mutate(year = as.numeric(as.character(year)),
                      value = value * 1E-9,
                      units = "Trillion$2015")

      rmap::map(data = pm.yll.EcoLoss.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_pm25_yll_ecoloss",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)

    }


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    pm.yll.EcoLoss<-dplyr::bind_rows(pm.yll.EcoLoss.list)
    m3_get_yll_pm25_ecoloss.output <<- pm.yll.EcoLoss
    return(invisible(pm.yll.EcoLoss))
  }

}


#' m3_get_mort_o3_ecoloss
#'
#'
#' Produce economic damages associated with premature mortality attributable to O3 (M6M) exposure based on the IER functions from Jerret et al (2009), consistent with the GBD 2016 study. The economic valuation takes as a base value the widely accepted Value of Statistical Life (VSL) of the OECD for 2005. This value, according to the literature ranges between US$1.8 and $4.5 million. The calculations for all regions are based on the  “unit value transfer approach” which adjusts the VSL according to their GDP and GDP growth rates. (Markandya et al 2018)
#' @source Jerrett, M., Burnett, R.T., Pope III, C.A., Ito, K., Thurston, G., Krewski, D., Shi, Y., Calle, E. and Thun, M., 2009. Long-term ozone exposure and mortality. New England Journal of Medicine, 360(11), pp.1085-1095.//// Markandya, A., Sampedro, J., Smith, S.J., Van Dingenen, R., Pizarro-Irizar, C., Arto, I. and González-Eguino, M., 2018. Health co-benefits from air pollution and mitigation costs of the Paris Agreement: a modelling study. The Lancet Planetary Health, 2(3), pp.e126-e133.
#' @keywords module_3, VSL ,premature mortality, O3
#' @return Economic damages associated with mortality attributable to O3 (M6M) exposure for each TM5-FASST regions for all years (Million$2015). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param rdata_name Name of the RData file. It must contain the queries in a list
#' @param scen_name Name of the GCAM scenario to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_param Select the health function (GBD 2016 or Jerret et al 2009) and the Low/Med/High RR. By default = mort_o3_gbd2016_med
#' @param Damage_vsl_range Select the VSL to calculate the damages (Damage_vsl_med, Damage_vsl_low, or Damage_vsl_high)
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_mort_o3_ecoloss<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name,
                                 rdata_name = NULL, scen_name, final_db_year = 2100,
                                 mort_param = "mort_o3_gbd2016_med", Damage_vsl_range = "Damage_vsl_med",
                                 ssp = "SSP2", queries = "queries_rfasst.xml", saveOutput = T, map = F, anim = T, recompute = F){

  if (!recompute & exists('m3_get_mort_o3_ecoloss.output')) {
    return(m3_get_mort_o3_ecoloss.output)
  } else {

    all_years<-all_years[all_years <= final_db_year]

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

    fasstSubset<-fasstSubset %>%
      dplyr::mutate(subRegionAlt=as.character(subRegionAlt)) %>%
      dplyr::left_join(fasst_reg,by="subRegionAlt") %>%
      dplyr::select(-subRegion) %>%
      dplyr::rename(subRegion=fasst_region) %>%
      dplyr::mutate(subRegionAlt=as.factor(subRegionAlt))

    # Get Mortalities
    o3.mort<-m3_get_mort_o3(db_path, query_path, db_name, prj_name, scen_name, queries, ssp = ssp, saveOutput = F, final_db_year = final_db_year, recompute = recompute) %>%
      dplyr::select(region, year, disease, mort_param) %>%
      dplyr::rename(mort_o3 = mort_param)

    # Get gdp_pc
    gdp_pc<-get(paste0('gdp_pc.',ssp))

    #------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------
    # Economic valuation of premature mortality: Value of Statistical Life (VSL)
    vsl<-gdp_pc %>%
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
      dplyr::mutate(unit = "Million$2005")

    #------------------------------------------------------------------------------------
    o3.mort.EcoLoss<-o3.mort %>%
      gcamdata::left_join_error_no_match(vsl, by = c("region","year")) %>%
      dplyr::select(-scenario) %>%
      # Calculate the median damages
      dplyr::mutate(Damage_vsl_med = round(mort_o3 * vsl_med * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                    Damage_vsl_low = round(mort_o3 * vsl_lb * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                    Damage_vsl_high = round(mort_o3 * vsl_ub * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                    unit = "Million$2015") %>%
      dplyr::select(region, year, disease, Damage_vsl_range, unit)

    #------------------------------------------------------------------------------------
    # Write the output
    o3.mort.EcoLoss.list<-split(o3.mort.EcoLoss, o3.mort.EcoLoss$year)

    o3.mort.EcoLoss.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","O3_MORT_ECOLOSS_",scen_name,"_",unique(df$year),".csv"),row.names = F)
    }


    if(saveOutput == T){

      lapply(o3.mort.EcoLoss.list, o3.mort.EcoLoss.write)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    # If map=T, it produces a map with the calculated outcomes

    if(map == T){
      o3.mort.EcoLoss.map<-o3.mort.EcoLoss %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, Damage_vsl_range) %>%
        dplyr::rename(value = Damage_vsl_range) %>%
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

    o3.mort.EcoLoss<-dplyr::bind_rows(o3.mort.EcoLoss.list)
    m3_get_mort_o3_ecoloss.output <<- o3.mort.EcoLoss
    return(invisible(o3.mort.EcoLoss))
  }

}

#' m3_get_yll_o3_ecoloss
#'
#'
#' Produce Economic damages associated with YLLs attributable to O3 (M6M).The economic valuation takes as a base value the Value of Statistical Life Year (VSLY) for EU from Schlander et al (2017) and expands the value to other regions based on the“unit value transfer approach” which adjusts the VSLY according to their GDP and GDP growth rates. YLL-to-Mortalities ratios are based on TM5-FASST calculations. Premature mortalities are  based on the integrated exposure-response functions (IER) from Burnett et al (2014), consistent with the GBD 2016 study.
#' @keywords module_3, YLL, O3, VSLY
#' @source Schlander, M., Schaefer, R. and Schwarz, O., 2017. Empirical studies on the economic value of a Statistical Life Year (VSLY) in Europe: what do they tell us?. Value in Health, 20(9), p.A666.
#' @return Economic damages associated with YLLs attributable to O3 (M6M) exposure for each TM5-FASST regions for all years (Thous$2015). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param rdata_name Name of the RData file. It must contain the queries in a list
#' @param scen_name Name of the GCAM scenario to be processed
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

m3_get_yll_o3_ecoloss<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name,
                                rdata_name = NULL, scen_name, queries = "queries_rfasst.xml", final_db_year = 2100,
                                ssp = "SSP2", saveOutput = T, map = F, anim = T, mort_param = "mort_o3_gbd2016_med", recompute = F){

  if (!recompute & exists('m3_get_yll_o3_ecoloss.output')) {
    return(m3_get_yll_o3_ecoloss.output)
  } else {

    all_years<-all_years[all_years <= final_db_year]

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_o3_yll_ecoloss")) dir.create("output/maps/m3/maps_o3_yll_ecoloss")

    # Ancillary Functions
    `%!in%` = Negate(`%in%`)

    # Shape subset for maps
    fasstSubset <- rmap::mapCountries

    fasstSubset<-fasstSubset %>%
      dplyr::mutate(subRegionAlt=as.character(subRegionAlt)) %>%
      dplyr::left_join(fasst_reg,by="subRegionAlt") %>%
      dplyr::select(-subRegion) %>%
      dplyr::rename(subRegion=fasst_region) %>%
      dplyr::mutate(subRegionAlt=as.factor(subRegionAlt))

    # Get Mortalities
    o3.yll<-m3_get_yll_o3(db_path, query_path, db_name, prj_name, scen_name, queries, ssp = ssp, saveOutput = F,
                          final_db_year = final_db_year, mort_param = mort_param, recompute = recompute)

    # Get gdp_pc
    gdp_pc<-get(paste0('gdp_pc.',ssp))

    #------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------
    # Economic valuation years of life lost
    vsly<-gdp_pc %>%
      # calculate the adjustment factor
      dplyr::mutate(adj = (gdp_pc / gdp_eu_2005) ^ inc_elas_vsl) %>%
      # multiply the LB and the UB of the European VSL (in 2005$), and take the median value
      dplyr::mutate(vsly = adj * vsly_eu_2005) %>%
      dplyr::select(scenario, region, year, vsly) %>%
      dplyr::mutate(vsly = vsly * 1E-3) %>%
      dplyr::mutate(unit = "Thous$2005")
    #------------------------------------------------------------------------------------

    o3.yll.EcoLoss<-o3.yll %>%
      gcamdata::left_join_error_no_match(vsly, by = c("region","year")) %>%
      dplyr::select(-scenario) %>%
      dplyr::mutate(Damage_med = round(yll_o3 * vsly * gcamdata::gdp_deflator(2015, base_year = 2005), 0),
                    unit = "Thous$2015") %>%
      dplyr::select(region, year, disease, Damage_med, unit)

    #------------------------------------------------------------------------------------
    # Write the output
    o3.yll.EcoLoss.list<-split(o3.yll.EcoLoss,o3.yll.EcoLoss$year)


    o3.yll.EcoLoss.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/","m3/","O3_YLL_ECOLOSS_",scen_name,"_",unique(df$year),".csv"),row.names = F)
    }


    if(saveOutput == T){

      lapply(o3.yll.EcoLoss.list,o3.yll.EcoLoss.write)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    # If map=T, it produces a map with the calculated outcomes

    if(map==T){
      o3.yll.EcoLoss.map<-o3.yll.EcoLoss %>%
        dplyr::rename(subRegion = region)%>%
        dplyr::filter(subRegion != "RUE") %>%
        dplyr::select(subRegion, year, Damage_med) %>%
        dplyr::rename(value = Damage_med) %>%
        dplyr::mutate(year = as.numeric(as.character(year)),
                      value = value * 1E-6,
                      units = "Billion$2015")

      rmap::map(data = o3.yll.EcoLoss.map,
                shape = fasstSubset,
                folder ="output/maps/m3/maps_o3_yll_ecoloss",
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    o3.yll.EcoLoss<-dplyr::bind_rows(o3.yll.EcoLoss.list)
    m3_get_yll_o3_ecoloss.output <<- o3.yll.EcoLoss
    return(invisible(o3.yll.EcoLoss))
  }
}

