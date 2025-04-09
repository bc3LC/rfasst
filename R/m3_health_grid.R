#' m3_get_mort_grid_pm25
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
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param mort_param Method to compute the premature mortality estimates used in the plotting: GBD, GEMM, or FUSION
#' @param saveOutput Writes the emission files.By default=T
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @param map Produce the maps. By default=F
#' @param anim If set to T, produces multi-year animations. By default=T
#' @param recompute If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F
#' @param gcam_eur If set to T, considers the GCAM-Europe regions. By default=F
#' @param normalize Adjust the output to represent the number of deaths per 100K people. By default = F
#' @param downscale If set to T, produces gridded PM2.5 outputs and plots By default=F
#' @param saveRaster_grid If set to T, writes the raster file with weighted PM25 By default=F
#' @param agg_grid Re-aggregate (downscaled) gridded data to any provided geometries (shape file). For the moment, only "NUTS3" and "CTRY" are available
#' @param save_AggGrid If set to T, writes the raster file with the reaggregated PM25 By default=F
#' @importFrom magrittr %>%
#' @export

m3_get_mort_grid_pm25<-function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL, prj_name, prj = NULL,
                           scen_name, queries = "queries_rfasst.xml", final_db_year = 2100, mort_param = "GBD",
                           ssp = "SSP2", saveOutput = T, map = F, anim = T, recompute = F, gcam_eur = F,
                           normalize = F, downscale = F, saveRaster_grid = F, agg_grid = F, save_AggGrid = F){


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Assert that the parameters of the function are okay, or modify when necessary

    if(is.null(prj_name)) assertthat::assert_that(!is.null(prj), msg = 'Specify the project name or pass an uploaded project as parameter')

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------

    # Create the directories if they do not exist:
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/m3")) dir.create("output/m3")
    if (!dir.exists("output/m3/pm25_gridded")) dir.create("output/m3/pm25_gridded")
    if (!dir.exists("output/m3/pm25_gridded/EUR_grid")) dir.create("output/m3/pm25_gridded/EUR_grid")
    if (!dir.exists("output/m3/pm25_gridded/computed_data")) dir.create("output/m3/pm25_gridded/computed_data")
    if (!dir.exists("output/maps")) dir.create("output/maps")
    if (!dir.exists("output/maps/m3")) dir.create("output/maps/m3")
    if (!dir.exists("output/maps/m3/maps_pm25_mort")) dir.create("output/maps/m3/maps_pm25_mort")
    if (!dir.exists("output/maps/m3/maps_pm25_mort/EUR_grid")) dir.create("output/maps/m3/maps_pm25_mort/EUR_grid")

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

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    rlang::inform('Computing premature deaths ...')

    ######## rfasst original code
    all_years<-rfasst::all_years[rfasst::all_years <= min(final_db_year)]
    all_years<-all_years[all_years>2005]

    # Get grided population
    pop.all.grid_mat <- get(paste0('pop.all.grid_mat.',ssp), envir = asNamespace("gcamreport"))


    # Get PM2.5
    pm.pre_mat <- list()
    extent_raster <- terra::ext(-26.276, 40.215, 32.633, 71.141)
    for (yy in all_years) {
      pm.pre <- terra::rast(paste0('output/m2/pm25_gridded/raster_grid/',yy,'_pm25_fin_weighted.tif'))
      pm.pre <- terra::crop(pm.pre, extent_raster)
      pm.pre_mat[[as.character(yy)]] <- as.matrix(pm.pre)
    }

    # Population weights by age
    pop.all.ctry_nuts3.str.SSP_w <- get(paste0('pop.all.ctry_nuts3.str.',ssp), envir = asNamespace("gcamreport")) %>%
      dplyr::filter(sex == 'Both', !age %in% c("0-4","5-9","10-14","15-19","20-24")) %>% # only pop > 25y considered
      dplyr::group_by(scenario, region, year, sex, unit) %>%
      dplyr::mutate(total = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(w = value / total) %>%
      dplyr::select(region, year, age, w)


    # Get baseline mortality rates
    mort.rates<-calc_mort_rates(downscale, agg_grid)

    mort.rates_iso_pre <- rfasst::Regions_EUR %>%
      dplyr::rename(region = 'FASST region') %>%
      gcamreport::left_join_strict(mort.rates, by = 'region') %>%
      dplyr::left_join(rfasst::isonum, by = 'ISO3') %>%
      dplyr::mutate(ISO3 = dplyr::if_else(ISO3 == 'ROU','ROM',ISO3)) %>%
      dplyr::left_join(rfasst::ctry_nuts3_codes, by = c('ISO3','ISO2')) %>%
      dplyr::filter(sex == 'Both')

    mort.rates_iso <- mort.rates_iso_pre %>%
      dplyr::filter(age != '>25') %>%
      dplyr::mutate(year = as.character(year)) %>%
      dplyr::left_join(pop.all.ctry_nuts3.str.SSP_w,
                       by = c('year','age','NUTS3'='region'),
                       relationship = "many-to-many") %>%
      dplyr::mutate(rate = rate * w) %>%
      dplyr::group_by(region = NUTS3, year, disease) %>%
      dplyr::reframe(rate = sum(rate, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::mutate(age = '>25') %>%
      rbind(mort.rates_iso_pre %>%
              dplyr::filter(age == '>25') %>%
              dplyr::select(region = NUTS3, year, disease, rate, age))

    mort.rates_iso_sf <- rfasst::nuts3_sf %>%
      dplyr::select(region = geo, geometry) %>%
      dplyr::left_join(
        mort.rates_iso,
        by = "region"
      )
    mort.rates_iso_sf <- sf::st_sf(mort.rates_iso_sf, geometry = mort.rates_iso_sf$geometry)
    for (dd in na.omit(unique(mort.rates_iso_sf$disease))) {
      print(dd)
      mort.rates_mat_yy <- list()
      for (yy in na.omit(unique(mort.rates_iso_sf$year))) {
        print(yy)

        mort.rates_rast <- terra::rasterize(mort.rates_iso_sf %>%
                                              dplyr::filter(year == yy,
                                                            disease == dd),
                                            pm.pre, field = 'rate')
        mort.rates_mat_yy[[as.character(yy)]] <- as.matrix(mort.rates_rast)

      }
      save(mort.rates_mat_yy, file = paste0('output/m3/pm25_gridded/mort.rates_mat_',dd,'.RData'))
      rm(mort.rates_mat_yy);gc()
    }

    # Get average relative risk parameters (pop weighted)
    GBD <- raw.rr.gbd.param %>%
      dplyr::filter(age != '>25') %>%
      dplyr::left_join(pop.all.ctry_nuts3.str.SSP_w,
                       by = 'age',
                       relationship = "many-to-many") %>%
      dplyr::mutate(alpha = alpha * w,
                    beta = beta * w,
                    delta = delta * w) %>%
      dplyr::group_by(region, year, disease) %>%
      dplyr::reframe(alpha = sum(alpha),
                       beta = sum(beta),
                       delta = sum(delta),
                       zcf = zcf) %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::mutate(age = '>25') %>%
      rbind(raw.rr.gbd.param %>%
              dplyr::filter(age == '>25') %>%
              dplyr::cross_join(pop.all.ctry_nuts3.str.SSP_w %>%
                                 dplyr::select(region, year) %>%
                                 dplyr::distinct()))
    GBD_sf <- rfasst::nuts3_sf %>%
      dplyr::select(region = geo, geometry) %>%
      dplyr::left_join(
        GBD,
        by = "region"
    )
    GBD_sf <- sf::st_sf(GBD_sf, geometry = GBD_sf$geometry)
    for (dd in na.omit(unique(GBD_sf$disease))) {
      print(dd)
      if (file.exists(paste0('GBD_mat_',dd,'.RData'))) {
        assign(paste0('GBD_mat_',dd), get(load(paste0('output/m3/pm25_gridded/GBD_mat_',dd,'.RData'))))
      } else {
        GBD_mat_yy <- list()
        for (yy in na.omit(unique(GBD_sf$year))) {
          print(yy)

          GBD_mat_param <- list()
          for (param in c('alpha','beta','delta','zcf')) {

            GBD_rast <- terra::rasterize(GBD_sf %>%
                                           dplyr::filter(year == yy,
                                                         disease == dd),
                                         pm.pre, field = param)
            GBD_mat_param[[param]] <- as.matrix(GBD_rast)
          }

          GBD_mat_yy[[as.character(yy)]] <- GBD_mat_param
        }
        save(GBD_mat_yy, file = paste0('output/m3/pm25_gridded/GBD_mat_',dd,'.RData'))
        rm(GBD_mat_yy); gc()
      }

    }


    # Mortality units
    normalize_pm_mort <- dplyr::if_else(!normalize, '', '_norm_100k')

    for (dd in unique(GBD$disease)) {
      print(dd)
      pm.mort_yy <- list()
      GBD_tmp <- get(load(paste0('output/m3/pm25_gridded/GBD_mat_',dd,'.RData')))
      rm(list = c(paste0('GBD_mat_',dd))); gc()
      mort.rates_tmp <- get(load(paste0('output/m3/pm25_gridded/mort.rates_mat_',dd,'.RData')))
      rm(list = c(paste0('mort.rates_mat_yy'))); gc()

      for (yy in na.omit(unique(GBD_sf$year))) {
        print(yy)
        rr.gbd <- 1 + GBD_tmp[[as.character(yy)]]$alpha * (1 - exp(-GBD_tmp[[as.character(yy)]]$beta * (pmax(0, pm.pre_mat[[yy]] - GBD_tmp[[as.character(yy)]]$zcf) ^ GBD_tmp[[as.character(yy)]]$delta)))
        pm.mort.allages_mat_tmp <- (1 - 1/ rr.gbd) * mort.rates_tmp[[yy]] * pop.all.grid_mat[[yy]]
        if (normalize) {
          pm.mort.allages_mat_tmp <- pm.mort.allages_mat_tmp / pop.all.grid_mat[[yy]] * 1e6
        }
        pm.mort_yy[[as.character(yy)]] <- pm.mort.allages_mat_tmp
      }

      save(pm.mort_yy, file = paste0('output/m3/pm25_gridded/EUR_grid/pm.mort_mat_',dd,normalize_pm_mort,'.RData'))
      rm(pm.mort_yy); rm(GBD_tmp); rm(mort.rates_tmp); gc()
    }


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If map=T, it produces a map with the calculated outcomes

    if(map && downscale){


      pm.mort_yy <- get(load(paste0('output/m3/pm25_gridded/EUR_grid/pm.mort_mat_',dd,normalize_pm_mort,'.RData')))
      for (yy in all_years) {
        vec <- as.vector(pm.mort_yy[[yy]])
        pm.mort_rast <- terra::setValues(pm.pre, vec)
        pm.mort_rast_masked <- terra::ifel(pm.mort_rast < quantile(a, probs = 0.75, na.rm = TRUE), pm.mort_rast, NA)
        png(paste0('output/maps/m3/maps_pm25_mort/EUR_grid/pm_mortality_map_',yy,normalize_pm_mort,'.png'), width = 800, height = 600)
        terra::plot(pm.mort_rast)
        dev.off()
        png(paste0('output/maps/m3/maps_pm25_mort/EUR_grid/pm_mortality_map_',yy,normalize_pm_mort,'_masked.png'), width = 800, height = 600)
        terra::plot(pm.mort_rast_masked)
        dev.off()
      }

      cat('Maps saved at output/m3/pm25_gridded/EUR_grid')
    }


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output
    return(NULL)


}
