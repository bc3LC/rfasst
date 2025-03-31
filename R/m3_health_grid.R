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
                           normalize = F,
                           downscale = F, saveRaster_grid = F,
                           agg_grid = F, save_AggGrid = F){

  if (!recompute & exists('m3_get_mort_grid_pm25.output')) {
    return(m3_get_mort_grid_pm25.output)
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
    if (!dir.exists("output/maps/m3/maps_pm25_mort")) dir.create("output/maps/m3/maps_pm25_mort")

    y = 2020
    aa.1 = 20
    aa.2 = ">25"
    dd = 'copd'

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

    # Get population with age groups
    # TODO - sembla q hi ha hagut un pb al baixar-me els .tif. Cal re-baixar-los
    # TODO - dps cal aplicar els mort.rates a les regiones/ctrys .tif q tinc
    # TODO - tot a d'estar en les mateixes dimensions
    pop.all.str <- terra::rast(paste0('C:/Users/claudia.rodes/Documents/Inequality/ineq_ap/data/pop_rasters/global_f_',aa.1,'_2019_1km.tif'))
    # pop.all.str <- terra::rast('C:/Users/claudia.rodes/Documents/Inequality/ineq_ap/data/pop_rasters/rr_f_20.tif')
    # pop_filtered <- terra::mask(pop.all.str, pop.all.str < 50, maskvalue=FALSE)
    # pop_filtered <- terra::mask(pop.all.str, pop.all.str > 0, maskvalue=FALSE)
    terra::plot(pop.all.str)
    # Get baseline mortality rates
    mort.rates<-calc_mort_rates(downscale, agg_grid)

    # Get PM2.5
    pm.pre <- terra::rast(paste0('C:/Users/claudia.rodes/Documents/GitHub/rfasst_v2/output/m2/pm25_gridded/raster_grid/',y,'_pm25_fin_weighted.tif'))
    extent_raster <- terra::ext(-26.276, 40.215, 32.633, 71.141)
    pm.pre <- terra::crop(pm.pre, extent_raster)

    pm.pre_mask=as.matrix(pm.pre)
    terra::values(pm.pre)=as.vector(pm.pre_mask)

    pop.all.str.resampled <- terra::resample(pop.all.str, pm.pre, method = "bilinear")
    pop.all_sf <- terra::as.polygons(pop.all.str.resampled) %>%
      sf::st_as_sf() %>%
      dplyr::rename(pop = global_f_20_2019_1km)
    pop.all_mask=as.matrix(pop.all.str.resampled)
    terra::plot(pop.all.str.resampled)

    boundaries.countries <- terra::rast('C:/Users/claudia.rodes/Documents/GitHub/rfasst_v2/inst/extdata/global_level0_1km_2000_2020.tif')
    boundaries.countries.resampled <- terra::resample(boundaries.countries, pm.pre, method = "bilinear")
    isonum <- read.csv('inst/extdata/isonum.csv', stringsAsFactors = F, na.strings = "")
    isocodes <- read.csv('inst/extdata/isonum.csv')

    mort.rates_iso <- rfasst::Regions_EUR %>%
      dplyr::rename(region = 'FASST region') %>%
      gcamreport::left_join_strict(mort.rates, by = 'region') %>%
      dplyr::left_join(isonum, by = 'ISO3') %>%
      dplyr::filter(year == y, sex == 'Female', age %in% aa.2, disease == dd) %>%
      dplyr::select(ISO3, disease, age, sex, year, rate, ISONUM, region)


    boundaries.countries.resampled_sf <- terra::as.polygons(boundaries.countries.resampled) %>%
      sf::st_as_sf() %>%
      dplyr::rename(ISONUM = global_level0_1km_2000_2020)

    mort.rates_sf <- mort.rates_iso %>%
      dplyr::left_join(boundaries.countries.resampled_sf,
                       by = "ISONUM"
      )
    mort.rates_sf <- sf::st_sf(mort.rates_sf, geometry = mort.rates_sf$geometry)
    sf::st_write(mort.rates_sf, "mort.rates_sf_eur.shp", append = F)

    # mort.rates_rast <- terra::rasterize(mort.rates_sf, pm.pre, field = "rate")
    # terra::plot(mort.rates_rast)



    ######## rfasst original code
    all_years<-rfasst::all_years[rfasst::all_years <= min(final_db_year,
                                  max(as.numeric(as.character(y))))]


    # Get relative risk parameters
    GBD <- raw.rr.gbd.param
    GEMM <- raw.rr.gemm.param %>%
      rbind(c(">25", 0, 0, 0, 2.4, 0, "dm"))

    # Mortality units
    normalize_pm_mort <- dplyr::if_else(!normalize, 'pm_mort', 'pm_mort_norm_100k')

    m3_get_mort_pm25.output.list <- list()
    for (sc in scen_name) {

        GBD_tmp <- GBD %>%
          dplyr::filter(age == aa.2, disease == dd)
        GEMM_tmp <- GEMM %>%
          dplyr::filter(age == aa.2, disease == dd) %>%
          dplyr::rename(nu = un) %>%
          dplyr::mutate(across(c(theta, alpha, mu, nu, cf_pm), ~ as.numeric(as.character(.))))

        pm.pre_sf <- terra::as.polygons(pm.pre) %>%
          sf::st_as_sf() %>%
          dplyr::rename(pm.conc = layer)

        pm_sf <- pm.pre_sf %>%
          dplyr::mutate(GBD_rr = 1 + GBD_tmp$alpha * (1 - exp(-GBD_tmp$beta * (pmax(0, pm.conc - GBD_tmp$zcf) ^ GBD_tmp$delta)))) %>%
          dplyr::mutate(conc.zcf = pm.conc - as.numeric(GEMM_tmp$cf_pm)) %>%
          dplyr::mutate(GEMM_rr = dplyr::if_else(conc.zcf <= 0,
                                                 0,
                                                 exp(GEMM_tmp$theta * log(pmax(0, conc.zcf) / (GEMM_tmp$alpha + 1)) /
                                                       (1 + exp(-(pmax(0, conc.zcf) - GEMM_tmp$mu) / GEMM_tmp$nu))))) %>%
          dplyr::select(-conc.zcf)

        # pm.rr.pre <- pm_sf

      #------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------
      # First, adjust population
      # pop_fin_str <- pop.all.str %>%
      #   dplyr::mutate(pop_1K = value * 1E3,
      #                 unit = "1K",
      #                 year = as.numeric(year)) %>%
      #   dplyr::select(-scenario, -unit, -value)
#
#       pop_fin_allages <- pop.all.str.resampled %>%
#         dplyr::group_by(region, year, sex) %>%
#         dplyr::summarise(value = sum(value)) %>%
#         dplyr::ungroup() %>%
#         dplyr::mutate(pop_1K = value * 1E3,
#                       unit = "1K",
#                       year = as.numeric(year)) %>%
#         dplyr::select(-value)

      # Calculate premature mortalities
      pm.mort_sf <- pm_sf %>%
        dplyr::select(-pm.conc) %>%
        tidyr::pivot_longer(cols = dplyr::ends_with("rr"),
                            names_to = "rr",
                            values_to = "value") %>%
        tidyr::replace_na(list(value = 0))

      # pm.mort.str <- pm.mort.pre %>%
      #   dplyr::filter(disease %in% c("ihd", "stroke")) %>%
      #   dplyr::mutate(year = as.numeric(as.character(year))) %>%
      #   # add sex columns
      #   tidyr::uncount(weights = 3) %>%
      #   dplyr::mutate(sex = rep(unique(mort.rates$sex), length.out = dplyr::n())) %>%
      #   gcamdata::left_join_error_no_match(mort.rates, by = c('region', 'year', 'disease', 'age', 'sex')) %>%
      #   dplyr::left_join(pop_fin_str, by = c('region', 'year', 'age', 'sex')) %>% # rm regions whose population is not estimated by the SSPs
      #   dplyr::mutate(mort = (1 - 1/ value) * rate * pop_1K / 100,
      #                 mort = round(mort, 0),
      #                 mort = dplyr::if_else(is.na(mort), 0, mort)) %>%
      #   dplyr::mutate(mort_norm_100k = mort / pop_1K * 100) %>%
      #   dplyr::select(region, year, age, sex, disease, pm_mort = mort, pm_mort_norm_100k = mort_norm_100k, rr) %>%
      #   dplyr::mutate(rr = gsub("_rr", "", rr)) %>%
      #   # adjust missing value for dm in the GEMM model
      #   dplyr::mutate(pm_mort = dplyr::if_else(is.finite(pm_mort), pm_mort, 0),
      #                 pm_mort_norm_100k = dplyr::if_else(is.finite(pm_mort_norm_100k), pm_mort_norm_100k, 0)) %>%
      #   dplyr::select(-one_of(setdiff(c('pm_mort', 'pm_mort_norm_100k'), normalize_pm_mort))) %>%
      #   tidyr::pivot_wider(names_from = rr,
      #                      values_from = !!rlang::sym(normalize_pm_mort))

      pm.mort_sf <- sf::st_transform(pm.mort_sf, sf::st_crs(mort.rates_sf))
      pop.all_sf <- sf::st_transform(pop.all_sf, sf::st_crs(mort.rates_sf))
      pm.mort_df <- as.data.frame(pm.mort_sf)
      pop.all_df <- as.data.frame(pop.all_sf)
      merged1_df <- pm.mort_df %>%
        dplyr::left_join(pop.all_df, by = 'geometry')
      # merged1_sf <- sf::st_join(pop.all_sf, mort.rates_sf, join = sf::st_intersects)
      # merged2_sf <- sf::st_join(merged1_sf, pm.mort_sf, join = sf::st_intersects)

      pm.mort_rast <- raster::rasterize(pm_sf %>%
                                          dplyr::select(GBD_rr, geometry),
                                        pm.pre,
                                        field = 'GBD_rr')
      pop.all_rast <- raster::rasterize(pop.all_sf,
                                        pm.pre,
                                        field = 'pop')
      pm.mort.allages <- (1 - 1/ pm.pre) * pm.mort_rast * pop.all_rast
      pm.mort.allages[is.na(pm.mort.allages)] <- NA
      pm.mort.allages[pm.mort.allages < 0] <- NA
      pm.mort.allages2 <- round(pm.mort.allages, 0)
      # terra::plot(terra::mask(pm.mort.allages, pm.mort.allages <50, maskvalue=FALSE))

      pm.mort.allages <- pm.mort_sf %>%
        gcamdata::left_join_error_no_match(mort.rates_sf, by = c('region', 'year', 'disease', 'age', 'sex')) %>%
        dplyr::left_join(pop_fin_allages, by = c('region', 'year', 'sex')) %>% # rm regions whose population is not estimated by the SSPs
        dplyr::mutate(mort = (1 - 1/ value) * rate * pop_1K / 100,
                      mort = round(mort, 0)) %>%
        dplyr::mutate(mort_norm_100k = mort / pop_1K * 100) %>%
        dplyr::select(region, year, age, sex, disease, pm_mort = mort, pm_mort_norm_100k = mort_norm_100k, rr) %>%
        dplyr::mutate(rr = gsub("_rr", "", rr)) %>%
        # adjust missing value for dm in the GEMM model
        dplyr::mutate(pm_mort = dplyr::if_else(is.finite(pm_mort), pm_mort, 0),
                      pm_mort_norm_100k = dplyr::if_else(is.finite(pm_mort_norm_100k), pm_mort_norm_100k, 0)) %>%
        dplyr::select(-one_of(setdiff(c('pm_mort', 'pm_mort_norm_100k'), normalize_pm_mort))) %>%
        tidyr::pivot_wider(names_from = rr,
                           values_from = !!rlang::sym(normalize_pm_mort))


      # pm.mort.allages <- pm.mort.pre %>%
      #   dplyr::filter(disease %!in% c("ihd", "stroke")) %>%
      #   dplyr::mutate(year = as.numeric(as.character(year))) %>%
      #   # add sex columns
      #   tidyr::uncount(weights = 3) %>%
      #   dplyr::mutate(sex = rep(unique(mort.rates$sex), length.out = dplyr::n())) %>%
      #   gcamdata::left_join_error_no_match(mort.rates, by = c('region', 'year', 'disease', 'age', 'sex')) %>%
      #   dplyr::left_join(pop_fin_allages, by = c('region', 'year', 'sex')) %>% # rm regions whose population is not estimated by the SSPs
      #   dplyr::mutate(mort = (1 - 1/ value) * rate * pop_1K / 100,
      #                 mort = round(mort, 0)) %>%
      #   dplyr::mutate(mort_norm_100k = mort / pop_1K * 100) %>%
      #   dplyr::select(region, year, age, sex, disease, pm_mort = mort, pm_mort_norm_100k = mort_norm_100k, rr) %>%
      #   dplyr::mutate(rr = gsub("_rr", "", rr)) %>%
      #   # adjust missing value for dm in the GEMM model
      #   dplyr::mutate(pm_mort = dplyr::if_else(is.finite(pm_mort), pm_mort, 0),
      #                 pm_mort_norm_100k = dplyr::if_else(is.finite(pm_mort_norm_100k), pm_mort_norm_100k, 0)) %>%
      #   dplyr::select(-one_of(setdiff(c('pm_mort', 'pm_mort_norm_100k'), normalize_pm_mort))) %>%
      #   tidyr::pivot_wider(names_from = rr,
      #                      values_from = !!rlang::sym(normalize_pm_mort))


      pm.mort <- dplyr::bind_rows(pm.mort.allages,
                                  pm.mort.str)


      #----------------------------------------------------------------------
      #----------------------------------------------------------------------
      # Write results

      pm.mort<-pm.mort %>%
        dplyr::mutate(scenario = sc) %>%
        dplyr::arrange(year,disease,region)
      m3_get_mort_pm25.output.list <- append(m3_get_mort_pm25.output.list, list(pm.mort))

    }

    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Bind the results

    m3_get_mort_pm25.output <- dplyr::bind_rows(m3_get_mort_pm25.output.list)


    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # If saveOutput=T,  writes aggregated PM2.5 values per TM5-FASST region

    normalize_tag <- dplyr::if_else(normalize, '_norm100k', '')

    pm.mort.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/", "m3/", "PM25_MORT_", paste(scen_name, collapse = "-"), "_", unique(df$year), normalize_tag, "_", agg_grid, ".csv"), row.names = F)
    }

    pm.mort.agg.write<-function(df){
      df<-as.data.frame(df)
      write.csv(df,paste0("output/", "m3/", "PM25_MORT_AGG_", paste(scen_name, collapse = "-"), "_", unique(df$year), normalize_tag, "_", agg_grid, ".csv"), row.names = F)
    }

    if(saveOutput == T){

      lapply(split(m3_get_mort_pm25.output, pm.mort$year),pm.mort.write)

      pm.mort.agg <- m3_get_mort_pm25.output %>%
        dplyr::filter(sex == 'Both') %>%
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

    if(map && !downscale){

      pm.mort.agg <- m3_get_mort_pm25.output %>%
        dplyr::filter(sex == 'Both') %>%
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
                folder = paste0("output/maps/m3/maps_pm25_mort/",normalize_tag),
                ncol = 3,
                legendType = "pretty",
                background  = T,
                animate = anim)
    } else if(map && downscale == 'NUTS3'){
      if (!dir.exists("output/m3/pm25_gridded")) dir.create("output/m3/pm25_gridded")
      if (!dir.exists("output/m3/pm25_gridded/agg_NUTS3")) dir.create("output/m3/pm25_gridded/agg_NUTS3")

      # Global
      m3_get_mort_pm25.output_sf <- as(rfasst::ctry_nuts_sf, "SpatVector") %>%
        dplyr::left_join(m3_get_mort_pm25.output %>%
                           dplyr::group_by(region, year, scenario, sex) %>%
                           dplyr::summarise(FUSION = sum(FUSION, na.rm = T),
                                            GBD = sum(GBD, na.rm = T),
                                            GEMM = sum(GEMM, na.rm = T)) %>%
                           dplyr::ungroup() %>%
                           dplyr::select(id_code = region, year, all_of(mort_param), sex, scenario) %>%
                           dplyr::rename(value = all_of(mort_param)) %>%
                           dplyr::mutate(units = "Mortalities",
                                         year = as.numeric(as.character(year))) %>%
                           tibble::as_tibble(),
                         by = 'id_code')

      # Europe
      m3_get_mort_pm25.output_EUR_sf <- as(rfasst::nuts_europe_sf, "SpatVector") %>%
        dplyr::left_join(m3_get_mort_pm25.output %>%
                           dplyr::group_by(region, year, scenario, sex) %>%
                           dplyr::summarise(FUSION = sum(FUSION, na.rm = T),
                                            GBD = sum(GBD, na.rm = T),
                                            GEMM = sum(GEMM, na.rm = T)) %>%
                           dplyr::ungroup() %>%
                           dplyr::select(id_code = region, year, all_of(mort_param), sex, scenario) %>%
                           dplyr::rename(value = all_of(mort_param)) %>%
                           dplyr::mutate(units = "Mortalities",
                                         year = as.numeric(as.character(year))) %>%
                           tibble::as_tibble(),
                         by = 'id_code')


      for (y in unique(m3_get_mort_pm25.output$year)) {

        # Global
        plot_ctry_nuts <- ggplot2::ggplot(data = m3_get_mort_pm25.output_sf %>%
                                            dplyr::filter(year == y)) +
          tidyterra::geom_spatvector(ggplot2::aes(fill = value), size = 0.1) +
          ggplot2::scale_fill_distiller(palette = "OrRd", direction = 1) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.title = ggplot2::element_blank())

        ggplot2::ggsave(paste0(here::here(),"/output/m3/pm25_gridded/agg_NUTS3/", y, "_WORLD-NUTS3_PM25mort_avg", normalize_tag, ".pdf"), plot_ctry_nuts,
                        width = 500, height = 400, units = 'mm')


        # Europe
        plot_eur_nuts <- ggplot2::ggplot(data = m3_get_mort_pm25.output_EUR_sf %>%
                                           dplyr::filter(year == y)) +
          tidyterra::geom_spatvector(ggplot2::aes(fill = value), size = 0.1) +
          ggplot2::scale_fill_distiller(palette = "OrRd", direction = 1) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.title = ggplot2::element_blank())

        ggplot2::ggsave(paste0(here::here(),"/output/m3/pm25_gridded/agg_NUTS3/", y, "_EUR-NUTS3_PM25mort_avg", normalize_tag, ".pdf"), plot_eur_nuts,
                        width = 500, height = 400, units = 'mm')



      }
      cat('Maps saved at output/m3/pm25_gridded/agg_NUTS3')
    } else if(map && downscale == 'CTRY'){
      if (!dir.exists("output/m3/pm25_gridded")) dir.create("output/m3/pm25_gridded")
      if (!dir.exists("output/m3/pm25_gridded/agg_CTRY")) dir.create("output/m3/pm25_gridded/agg_CTRY")

      # Global
      m3_get_mort_pm25.output_sf <- as(rfasst::ctry_nuts_sf, "SpatVector") %>%
        dplyr::left_join(m3_get_mort_pm25.output %>%
                           dplyr::group_by(region, year, scenario, sex) %>%
                           dplyr::summarise(FUSION = sum(FUSION, na.rm = T),
                                            GBD = sum(GBD, na.rm = T),
                                            GEMM = sum(GEMM, na.rm = T)) %>%
                           dplyr::ungroup() %>%
                           dplyr::select(id_code = region, year, all_of(mort_param), sex, scenario) %>%
                           dplyr::rename(value = all_of(mort_param)) %>%
                           dplyr::mutate(units = "Mortalities",
                                         year = as.numeric(as.character(year))) %>%
                           tibble::as_tibble(),
                         by = 'id_code')

      # Europe
      m3_get_mort_pm25.output_EUR_sf <- as(rfasst::ctry_nuts_sf %>%
                                             dplyr::filter(region_type == 'CTRY'),
                                           "SpatVector") %>%
        dplyr::left_join(m3_get_mort_pm25.output %>%
                           dplyr::group_by(region, year, scenario, sex) %>%
                           dplyr::summarise(FUSION = sum(FUSION, na.rm = T),
                                            GBD = sum(GBD, na.rm = T),
                                            GEMM = sum(GEMM, na.rm = T)) %>%
                           dplyr::ungroup() %>%
                           dplyr::select(id_code = region, year, all_of(mort_param), sex, scenario) %>%
                           dplyr::rename(value = all_of(mort_param)) %>%
                           dplyr::mutate(units = "Mortalities",
                                         year = as.numeric(as.character(year))) %>%
                           tibble::as_tibble(),
                         by = 'id_code')


      for (y in unique(m3_get_mort_pm25.output$year)) {

        # Global
        plot_ctry_nuts <- ggplot2::ggplot(data = m3_get_mort_pm25.output_sf %>%
                                            dplyr::filter(year == y)) +
          tidyterra::geom_spatvector(ggplot2::aes(fill = value), size = 0.1) +
          ggplot2::scale_fill_distiller(palette = "OrRd", direction = 1) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.title = ggplot2::element_blank())

        ggplot2::ggsave(paste0(here::here(),"/output/m3/pm25_gridded/agg_CTRY/", y, "_WORLD-CTRY_PM25mort_avg", normalize_tag, ".pdf"), plot_ctry_nuts,
                        width = 500, height = 400, units = 'mm')


        # Europe
        plot_eur_nuts <- ggplot2::ggplot(data = m3_get_mort_pm25.output_EUR_sf %>%
                                           dplyr::filter(year == y)) +
          tidyterra::geom_spatvector(ggplot2::aes(fill = value), size = 0.1) +
          ggplot2::scale_fill_distiller(palette = "OrRd", direction = 1) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.title = ggplot2::element_blank())

        ggplot2::ggsave(paste0(here::here(),"/output/m3/pm25_gridded/agg_CTRY/", y, "_EUR-CTRY_PM25mort_avg", normalize_tag, ".pdf"), plot_eur_nuts,
                        width = 500, height = 400, units = 'mm')



      }
      cat('Maps saved at output/m3/pm25_gridded/agg_CTRY')
    }



    #----------------------------------------------------------------------
    #----------------------------------------------------------------------
    # Return output
    return(invisible(m3_get_mort_pm25.output))

  }
}
