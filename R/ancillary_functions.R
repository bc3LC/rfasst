#' left_join_strict
#'
#' ADAPTED FROM GCAMREPORT (https://github.com/bc3LC/gcamreport)
#' A restrictive version of \code{\link{left_join}} that ensures that all keys in the left dataset have corresponding matches in the right dataset.
#' If any rows in the left dataset do not have matching keys in the right dataset, the function will throw an error.
#'
#' @param left_df A data frame. The left dataset in the join.
#' @param right_df A data frame. The right dataset in the join.
#' @param by A character vector of variables to join by. If `NULL`, the function will use all common variables.
#' @return A data frame resulting from the left join. If any rows in `left_df` do not have matching keys in `right_df`, an error is thrown.
#' @export
left_join_strict <- function(left_df, right_df, by = NULL) {
  # Perform the left join
  result <- dplyr::left_join(left_df, right_df, by = by)

  # Identify unmatched rows (rows with NA in any of the columns from right_df)
  unmatched <- result %>%
    dplyr::filter(dplyr::if_any(-one_of(names(left_df)), is.na))

  # Check if there are any unmatched rows
  if (nrow(unmatched) > 0) {
    left_join_strict_details <- unique(unmatched %>%
                                         dplyr::select(by))
    left_join_strict_details <<- left_join_strict_details
    stop(sprintf("Error: Some rows in the left dataset do not have matching keys in the right dataset. Type `left_join_strict_details` to see the full log.",
                 paste(capture.output(print(left_join_strict_details)), collapse = "\n")))
  }

  return(result)
}



#' if_complex
#'
#' Do an if_else to assign data1 or data2 to a dataset
#' @param condition: condifiton to be satisfied to assing data1
#' @param data1: dataset1
#' @param data2: dataset2
#' @return dataset accroding to the given conditon
#' @export
if_complex = function(condition, data1, data2) {
  if (condition) {
    return(invisible(data1))
  } else {
    return(invisible(data2))
  }
}


#' check_byu
#'
#' Check if the data contains GCAM results after the BYU, i.e., with the year 2021
#' instead of the year 2020, and change the year to match the socioeconomic preloaded data
#' @param data: dataset
#' @return dataset with the year 2020 (corresponding to the estimated 2021 if present).
#' @export
check_byu = function(data) {
  if ('year' %in% colnames(data) & "2021" %in% as.character(unique(data$year))) {
    data <- data %>%
      dplyr::mutate(year = dplyr::if_else(year == 2021, 2020, year))
  }

  return(data)
}


#' check_gcamversion
#'
#' Check if the data contains GCAM results from GCAM8.2 or newer versions to arrange
#' the regions (e.g., Ukraine)
#' @param tmp Dummy dataset (any query from the prj file) to check if the BYU (and regions' update) is implemented in the GCAM considered output.
#' @param gcam_eur If set to T, considers the GCAM-Europe regions.
#' @return list of GCAM version region-dependent constants
#' @export
check_gcamversion = function(tmp, gcam_eur) {

  # the BYU coincides with the GCAM8.2 version release, that has the difference in the regions' aggregation
  if (!gcam_eur & 'year' %in% colnames(tmp) & "2021" %in% as.character(unique(tmp$year))) {
    list_result <- list(
      rfasst::GCAM_reg_vgt8.2,
      rfasst::Regions_vgt8.2,
      rfasst::Percen_vgt8.2,
      rfasst::d.weight.gcam_vgt8.2
    )
  } else if (gcam_eur) {
    list_result <- list(
      rfasst::GCAM_reg_EUR,
      rfasst::Regions_EUR,
      rfasst::Percen_EUR,
      rfasst::d.weight.gcam_EUR
    )
  } else {
    list_result <- list(
      rfasst::GCAM_reg_vlt8.2,
      rfasst::Regions_vlt8.2,
      rfasst::Percen_vlt8.2,
      rfasst::d.weight.gcam_vlt8.2
    )
  }

  return(list_result)
}


#' load_prj
#'
#' @description Load/Create prj file
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param scen_name Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @return loaded project
load_prj <- function(db_path = NULL, query_path = "./inst/extdata", db_name = NULL,
                     prj_name, prj = NULL, scen_name, queries = "queries_rfasst.xml") {
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

    } else {
      rlang::inform('Loading project ...')
      prj <- rgcam::loadProject(prj_name)
    }
  } else {
    rlang::inform('Project already loaded ...')
  }

  return(prj)
}

#' clean_pkg_outputs
#'
#' List of global variables generated by the pkg to be erased
#' @return List with the variables to be erased. Type "rm(list = clean_pkg_outputs())" to remove them
#' @export
clean_pkg_outputs = function() {
  vars_all = c('m1_emissions_rescale.output',
           'm2_get_conc_pm25.output',
           'm2_get_conc_o3.output',
           'm2_get_conc_m6m.output',
           'm2_get_conc_aot40.output',
           'm2_get_conc_mi.output',
           'm3_get_mort_pm25.output',
           'm3_get_mort_pm25_ecoloss.output',
           'm3_get_yll_pm25.output',
           'm3_get_yll_pm25_ecoloss.output',
           'm3_get_daly_pm25.output',
           'm3_get_mort_o3.output',
           'm3_get_mort_o3_ecoloss.output',
           'm3_get_yll_o3.output',
           'm3_get_yll_o3_ecoloss.output',
           'm3_get_daly_o3.output',
           'calc_prod_gcam.output',
           'calc_price_gcam.output',
           'calc_rev_gcam.output',
           'm4_get_ryl_aot40.output',
           'm4_get_ryl_mi.output',
           'm4_get_prod_loss.output',
           'm4_get_rev_loss.output')

  vars_to_rm = c()
  for (v in vars_all) {
    if (exists(v)) {
      vars_to_rm = c(vars_to_rm, v)
    }
  }

  return(vars_to_rm)
}




#' data_query
#'
#' Add nonCO2 large queries
#' @param db_path: path of the database
#' @param db_name: name of the database
#' @param prj_name: name of the project
#' @param scenarios: name of the scenarios to be considered
#' @param type: either 'nonCO2 emissions by region' or 'nonCO2 emissions by sector'
#' @param query_path Path to the query file
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @return dataframe with the data from the query
#' @export
data_query = function(type, db_path, db_name, prj_name, scenarios,
                      query_path = 'inst/extdata', queries = 'queries_rfasst_nonCO2.xml') {
  dt = data.frame()
  xml <- xml2::read_xml(file.path(query_path,queries))
  qq <- xml2::xml_find_first(xml, paste0("//*[@title='", type, "']"))

  # retrive nonCO2 pollutants
  for (sc in scenarios) {
    emiss_list <- unique(rfasst::my_pol$Pollutant)
    emiss_list <- emiss_list[!(emiss_list %in% c('CO2', 'CO2_FUG'))]
    while (length(emiss_list) > 0) {
      current_emis = emiss_list[1:min(21,length(emiss_list))]
      qq_sec = gsub("current_emis", paste0("(@name = '", paste(current_emis, collapse = "' or @name = '"), "')"), qq)

      prj_tmp = rgcam::addSingleQuery(
        conn = rgcam::localDBConn(db_path,
                                  db_name,migabble = FALSE),
        proj = prj_name,
        qn = type,
        query = qq_sec,
        scenario = sc,
        regions = NULL,
        clobber = TRUE,
        transformations = NULL,
        saveProj = FALSE,
        warn.empty = FALSE
      )

      tmp = data.frame(prj_tmp[[sc]][type])
      if (nrow(tmp) > 0) {
        dt = dplyr::bind_rows(dt,tmp)
      }
      rm(prj_tmp)

      if (length(emiss_list) > 21) {
        emiss_list <- emiss_list[(21 + 1):length(emiss_list)]
      } else {
        emiss_list = c()
      }
    }
  }
  # Rename columns
  new_colnames <- sub(".*\\.(.*)", "\\1", names(dt))
  names(dt) <- new_colnames

  return(dt)
}




calc_pop_grid <- function(ssp = 'SSP2') {
  extent_raster <- terra::ext(-26.276, 40.215, 32.633, 71.141)

  pm.pre <- terra::rast(paste0('inst/extdata/pm25_weights_rast_rfasstReg.tif'))
  pm.pre <- terra::crop(pm.pre, extent_raster)

  pop.all.grid_mat <- list()
  for (yy in rfasst::all_years[rfasst::all_years > 2005]) {
    if (yy %in% c(2010, 2020)) {
      # # https://hub.worldpop.org/geodata/summary?id=24776
      pop.pre <- terra::rast(paste0('inst/extdata/pop_rasters/ppp_',yy,'_1km_Aggregated.tif'))
    } else if (yy > 2020) {
      # https://www.nature.com/articles/s41597-022-01675-x#Sec9;
      # https://figshare.com/articles/dataset/Projecting_1_km-grid_population_distributions_from_2020_to_2100_globally_under_shared_socioeconomic_pathways/19608594/3?file=34829370
      pop.pre <- terra::rast(paste0('inst/extdata/data/pop_rasters/',ssp,'/',ssp,'_',yy,'.tif'))
    }
    pop.pre <- terra::crop(pop.pre, extent_raster)
    pop.all.str.resampled <- terra::resample(pop.pre, pm.pre, method = "bilinear")
    pop.all.grid_mat[[as.character(yy)]] <- as.matrix(pop.all.str.resampled)
  }
  return(pop.all.grid_mat)
}


#' calc_pop
#'
#' Get population data and shares of population under 5 Years and above 30 Years from the SSP database (SSP_database_v9).To be consistent we make use of the IIASA-WIC Model/scenarios. Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth" for this region.
#' @source  https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @keywords socioeconomics, population
#' @return Population and population shares (<5Y; >30Y) for TM5-FASST regions for all years
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @importFrom magrittr %>%
#' @export

calc_pop<-function(ssp = "SSP2"){

  # Ancillary Functions
  `%!in%` = Negate(`%in%`)

  # First, we read in the population data.
  ssp.data<-raw.ssp.data %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
           grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT)

  pop<-tibble::as_tibble(ssp.data) %>%
    dplyr::filter(variable == "Population") %>%
    dplyr::rename(pop_tot = value) %>%
    #add FASST regions and aggregate the values to those categories:
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>% dplyr::rename(region = subRegionAlt),
                             by = "region") %>%
    dplyr::select(-region) %>%
    dplyr::rename(region = fasst_region) %>%
    dplyr::group_by(model, scenario, region, year, unit) %>%
    dplyr::summarise(pop_tot = sum(pop_tot)) %>%
    dplyr::ungroup()

  # We need to calculate by country and period the proportions for POP>30Y and POP<5Y, which are going to be needed as input in health functions
  pop.5<-ssp.data %>%
    dplyr::filter(variable %in% c("Population|Female|Aged0-4", "Population|Male|Aged0-4")) %>%
    dplyr::group_by(model, scenario, region, year, unit) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()%>%
    dplyr::rename(pop_5 = value) %>%
    #add FASST regions and add the values to those categories:
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>% dplyr::rename(region = subRegionAlt),
                             by = "region") %>%
    dplyr::select(-region) %>%
    dplyr::rename(region = fasst_region) %>%
    dplyr::group_by(model, scenario, region, year, unit) %>%
    dplyr::summarise(pop_5 = sum(pop_5)) %>%
    dplyr::ungroup()

  pop.30<-ssp.data %>%
    dplyr::filter(variable %in% c("Population|Female|Aged30-34","Population|Male|Aged30-34",
                           "Population|Female|Aged35-39","Population|Male|Aged35-39",
                           "Population|Female|Aged40-44","Population|Male|Aged40-44",
                           "Population|Female|Aged45-49","Population|Male|Aged45-49",
                           "Population|Female|Aged50-54","Population|Male|Aged50-54",
                           "Population|Female|Aged55-59","Population|Male|Aged55-59",
                           "Population|Female|Aged60-64","Population|Male|Aged60-64",
                           "Population|Female|Aged65-69","Population|Male|Aged65-69",
                           "Population|Female|Aged70-74","Population|Male|Aged70-74",
                           "Population|Female|Aged75-79","Population|Male|Aged75-79",
                           "Population|Female|Aged80-84","Population|Male|Aged80-84",
                           "Population|Female|Aged85-89","Population|Male|Aged85-89",
                           "Population|Female|Aged90-94","Population|Male|Aged90-94",
                           "Population|Female|Aged95-99","Population|Male|Aged95-99",
                           "Population|Female|Aged100+","Population|Male|Aged100+")) %>%
    dplyr::group_by(model, scenario, region, year, unit) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()%>%
    dplyr::rename(pop_30 = value) %>%
    #add FASST regions and add the values to those categories:
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>% dplyr::rename(region = subRegionAlt),
                             by = "region") %>%
    dplyr::select(-region) %>%
    dplyr::rename(region = fasst_region) %>%
    dplyr::group_by(model, scenario, region, year, unit) %>%
    dplyr::summarise(pop_30 = sum(pop_30)) %>%
    dplyr::ungroup()

  # We calculate the non-used pop 5-30Y just to check everything matches
  pop.5.30<-ssp.data %>%
    dplyr::filter(variable %in% c("Population|Female|Aged5-9","Population|Male|Aged5-9",
                           "Population|Female|Aged10-14","Population|Male|Aged10-14",
                           "Population|Female|Aged15-19","Population|Male|Aged15-19",
                           "Population|Female|Aged20-24","Population|Male|Aged20-24",
                           "Population|Female|Aged25-29","Population|Male|Aged25-29")) %>%
    dplyr::group_by(model, scenario, region, year, unit) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(pop_other = value) %>%
    #add FASST regions and add the values to those categories:
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>% dplyr::rename(region = subRegionAlt),
                             by = "region") %>%
    dplyr::select(-region) %>%
    dplyr::rename(region = fasst_region) %>%
    dplyr::group_by(model, scenario, region, year, unit) %>%
    dplyr::summarise(pop_other = sum(pop_other)) %>%
    dplyr::ungroup()


  pop.all<- tibble::as_tibble(pop) %>%
    gcamdata::left_join_error_no_match(pop.5,by = c("model", "scenario", "region", "unit", "year")) %>%
    gcamdata::left_join_error_no_match(pop.30,by = c("model", "scenario", "region", "unit", "year")) %>%
    gcamdata::left_join_error_no_match(pop.5.30,by = c("model", "scenario", "region", "unit", "year")) %>%
    dplyr::mutate(pop_tot_check = pop_5 + pop_30 + pop_other,
           diff = pop_tot - pop_tot_check) %>%
    dplyr::mutate(perc_pop_5 = pop_5 / pop_tot,
           perc_pop_30 = pop_30 / pop_tot) %>%
    dplyr::mutate(scenario = ssp) %>%
    dplyr::select(scenario, region, year, unit, pop_tot, perc_pop_5, perc_pop_30)


  # Taiwan is not included in the database, so we use population projections from OECD Env-Growth.
  # We use China's percentages for under 5 and over 30 (to be updated)

  perc.china<-pop.all %>%
    dplyr::filter(region == "CHN") %>%
    dplyr::select(year, perc_pop_5, perc_pop_30)

  twn.pop<-tibble::as_tibble(raw.twn.pop) %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
           grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT) %>%
    dplyr::filter(variable == "Population") %>%
    dplyr::rename(pop_tot = value) %>%
    #add FASST regions and add the values to those categories:
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>% dplyr::rename(region = subRegionAlt),
                             by = "region") %>%
    dplyr::select(-region) %>%
    dplyr::rename(region = fasst_region) %>%
    dplyr::group_by(model, scenario, region, year, unit) %>%
    dplyr::summarise(pop_tot = sum(pop_tot)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-model) %>%
    dplyr::mutate(scenario = ssp) %>%
    gcamdata::left_join_error_no_match(perc.china, by = "year")

  # We add twn to the database
  pop.all<-dplyr::bind_rows(pop.all, twn.pop)

  # In addition we don't have population values for RUE, so we divide the population between these regions using percentages
  # Following TM5-FASST, we assume that 76.7% of population is assigned to RUS, while the remaining 23.3% to RUE.
  # These percentages are loaded in the configuration file and can be adapted
  pop_rus<-pop.all %>% dplyr::filter(region == "RUS") %>% dplyr::mutate(pop_tot = pop_tot * perc_pop_rus)

  pop_rue<-pop.all %>% dplyr::filter(region == "RUS") %>% dplyr::mutate(region = "RUE") %>% dplyr::mutate(pop_tot = pop_tot * perc_pop_rue)

  # We add rus and rue to the database
  pop.all<-pop.all %>%
    dplyr::filter(region != "RUS") %>%
    dplyr::bind_rows(pop_rus) %>%
    dplyr::bind_rows(pop_rue)

  invisible(pop.all)

}

#' calc_pop_rfasst_reg_str
#'
#' Get population data and shares of population for all population segments.
#' To be consistent we make use of the IIASA-WIC Model/scenarios. rfasst regions.
#' Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth" for this region.
#' @source  https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @keywords socioeconomics, population
#' @return Population and population shares (<5Y; >30Y) for TM5-FASST regions for all years
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @importFrom magrittr %>%
#' @export

calc_pop_rfasst_reg_str<-function(ssp = "SSP2"){

  # Ancillary Functions
  `%!in%` = Negate(`%in%`)

  # First, we read in the population data.
  ssp.data<-raw.ssp.data %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
                  grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT)

  pop<-tibble::as_tibble(ssp.data) %>%
    dplyr::filter(grepl("Population", variable)) %>%
    dplyr::mutate(variable = gsub("-", "and", variable)) %>%
    dplyr::left_join(age_str_mapping, by = 'variable') %>%
    dplyr::filter(complete.cases(age)) %>%
    dplyr::filter(!grepl("Edu", variable)) %>%
    dplyr::mutate(age = dplyr::if_else(age == "95-99" | age == "100", "95+", age)) %>%
    dplyr::mutate(sex = dplyr::case_when(
      stringr::str_detect(variable, "\\|Female\\|") ~ "Female",
      stringr::str_detect(variable, "\\|Male\\|") ~ "Male",
      TRUE ~ "Both"
    )) %>%
    dplyr::select(-variable) %>%
    dplyr::group_by(model, scenario, region, age, sex, unit, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    #add FASST regions and aggregate the values to those categories:
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>% dplyr::rename(region = subRegionAlt),
                                       by = "region") %>%
    dplyr::select(-region) %>%
    dplyr::rename(region = fasst_region) %>%
    dplyr::group_by(model, scenario, region, age, sex, year, unit) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  # Taiwan is not included in the database, so we use population projections from OECD Env-Growth.
  # We use China's percentages

  perc.china.str<-pop %>%
    dplyr::filter(region == "CHN") %>%
    dplyr::group_by(model, scenario, region, year) %>%
    dplyr::mutate(pop_tot = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(share = value / pop_tot) %>%
    dplyr::select(model, scenario, age, sex, year, share)

  twn.pop<-tibble::as_tibble(raw.twn.pop) %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
                  grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT) %>%
    dplyr::filter(variable == "Population") %>%
    dplyr::rename(pop_tot = value) %>%
    #add FASST regions and add the values to those categories:
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>% dplyr::rename(region = subRegionAlt),
                                       by = "region") %>%
    dplyr::select(-region) %>%
    dplyr::rename(region = fasst_region) %>%
    dplyr::group_by(model, scenario, region, year, unit) %>%
    dplyr::summarise(pop_tot = sum(pop_tot)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-model) %>%
    dplyr::mutate(scenario = ssp) %>%
    gcamdata::repeat_add_columns(tibble::tibble(age = unique(perc.china.str$age))) %>%
    gcamdata::repeat_add_columns(tibble::tibble(sex = unique(perc.china.str$sex))) %>%
    gcamdata::left_join_error_no_match(perc.china.str %>% dplyr::select(-scenario), by = c("year", "age", "sex")) %>%
    dplyr::mutate(value = pop_tot * share,
                  model = "IIASA-WiC POP",
                  scenario = "SSP2_v9_130115") %>%
    dplyr::select(model, scenario, region, age, sex, year, unit, value)

  # We add twn to the database
  pop <-dplyr::bind_rows(pop, twn.pop)

  # In addition we don't have population values for RUE, so we divide the population between these regions using percentages
  # Following TM5-FASST, we assume that 76.7% of population is assigned to RUS, while the remaining 23.3% to RUE.
  # These percentages are loaded in the configuration file and can be adapted
  pop_rus.str<- pop %>% dplyr::filter(region == "RUS") %>% dplyr::mutate(value = value * perc_pop_rus)

  pop_rue.str<-pop %>% dplyr::filter(region == "RUS") %>% dplyr::mutate(region = "RUE") %>% dplyr::mutate(value = value * perc_pop_rue)

  # We add rus and rue to the database
  pop.tot <- pop %>%
    dplyr::filter(region != "RUS") %>%
    dplyr::bind_rows(pop_rus.str) %>%
    dplyr::bind_rows(pop_rue.str) %>%
    dplyr::mutate(scenario = ssp) %>%
    dplyr::select(scenario, region, year, age, sex, unit, value)

  # Compute total pop (sex = Both)
  pop.both <- pop.tot %>%
    dplyr::group_by(scenario, region, year, age, unit) %>%
    dplyr::summarise(value = sum(value),
                     sex = 'Both') %>%
    dplyr::ungroup()

  pop <- dplyr::bind_rows(
    pop.tot,
    pop.both
  )

  invisible(pop)

}


#' calc_pop_ctry_nuts3_str
#'
#' Get population data and shares of population for all population segments.
#' To be consistent we make use of the IIASA-WIC Model/scenarios. NUTS3 codes for the European region and ISO3 codes for the ROW.
#' Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth" for this region.
#' @source  https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @keywords socioeconomics, population
#' @return Population and population shares (<5Y; >30Y) for NUTS3 regions for all years
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @importFrom magrittr %>%
#' @export

calc_pop_ctry_nuts3_str<-function(ssp = "SSP2"){


  # Ancillary Functions
  `%!in%` = Negate(`%in%`)

  # First, we read in the population data.
  ssp.data<-raw.ssp.data %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
                  grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT)

  pop<-tibble::as_tibble(ssp.data) %>%
    dplyr::filter(grepl("Population", variable)) %>%
    dplyr::mutate(variable = gsub("-", "and", variable)) %>%
    dplyr::left_join(age_str_mapping, by = 'variable') %>%
    dplyr::filter(complete.cases(age)) %>%
    dplyr::filter(!grepl("Edu", variable)) %>%
    dplyr::mutate(age = dplyr::if_else(age == "95-99" | age == "100", "95+", age)) %>%
    dplyr::mutate(sex = dplyr::case_when(
      stringr::str_detect(variable, "\\|Female\\|") ~ "Female",
      stringr::str_detect(variable, "\\|Male\\|") ~ "Male",
      TRUE ~ "Both"
    )) %>%
    dplyr::select(-variable) %>%
    dplyr::group_by(model, scenario, region, age, sex, unit, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # add NUTS3 regions and aggregate the values to those categories:
    dplyr::rename('ISO3' = 'region') %>%
    dplyr::mutate(ISO3 = dplyr::if_else(ISO3 == 'ROU', 'ROM', ISO3)) %>%  # fix Romania ISO3 code
    dplyr::left_join(rfasst::ctry_nuts3_codes, # left_join since NUTS3 increments the nº of rows
                     by = "ISO3", relationship = "many-to-many") %>%
    dplyr::select(-ISO3, -ISO2) %>%
    dplyr::filter(!is.na(NUTS3)) %>%  # rm overseas regions
    # add pop-weights to downscale to NUTS3 level
    dplyr::left_join(weight.nuts.pop.sex %>%
                       tibble::as_tibble() %>%
                       dplyr::filter(age != 'TOTAL', sex != 'T') %>%
                       dplyr::mutate(sex = dplyr::if_else(sex == 'M', 'Male',
                                                          dplyr::if_else(sex == 'F', 'Female',
                                                                         'T'))) %>%
                       dplyr::mutate(age = gsub("^Y", "", age)) %>%
                       dplyr::select(age, sex, NUTS3 = geo, weight),
                     by = c('NUTS3','age','sex')) %>%
    dplyr::mutate(weight = dplyr::if_else(is.na(weight) & nchar(NUTS3) == 5, 0, weight)) %>% # if NUTS3 but empty weight
    dplyr::mutate(value = dplyr::if_else(!is.na(weight), value * weight, value)) %>%
    dplyr::group_by(model, scenario, region = NUTS3, age, sex, year, unit) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  # Taiwan is not included in the database, so we use population projections from OECD Env-Growth.
  # We use China's percentages
  perc.china.str<-pop %>%
    dplyr::filter(region == "CHN") %>%
    # aggregate all NUTS3 to have the values at country level
    dplyr::group_by(model, scenario, region, age, sex, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute the share by age & sex
    dplyr::group_by(model, scenario, region, year) %>%
    dplyr::mutate(pop_tot = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(share = value / pop_tot) %>%
    dplyr::select(model, scenario, age, sex, year, share)

  twn.pop<-tibble::as_tibble(raw.twn.pop) %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
                  grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT) %>%
    dplyr::filter(variable == "Population") %>%
    dplyr::rename(pop_tot = value) %>%
    # add NUTS3 regions and aggregate the values to those categories:
    dplyr::rename('ISO3' = 'region') %>%
    dplyr::left_join(rfasst::ctry_nuts3_codes, # left_join since NUTS3 increments the nº of rows
                     by = "ISO3", relationship = "many-to-many") %>%
    dplyr::select(-ISO3, -ISO2) %>%
    dplyr::filter(!is.na(NUTS3)) %>% # rm Micronesia
    dplyr::group_by(model, scenario, region = NUTS3, year, unit) %>%
    dplyr::summarise(pop_tot = sum(pop_tot)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-model) %>%
    dplyr::mutate(scenario = ssp) %>%
    gcamdata::repeat_add_columns(tibble::tibble(age = unique(perc.china.str$age))) %>%
    gcamdata::repeat_add_columns(tibble::tibble(sex = unique(perc.china.str$sex))) %>%
    gcamdata::left_join_error_no_match(perc.china.str %>% dplyr::select(-scenario), by = c("year", "age", "sex")) %>%
    dplyr::mutate(value = pop_tot * share,
                  model = "IIASA-WiC POP",
                  scenario = "SSP2_v9_130115") %>%
    dplyr::select(model, scenario, region, age, sex, year, unit, value)

  # We add twn to the database
  pop <-dplyr::bind_rows(pop, twn.pop)

  # Clean the database
  pop.tot <- pop %>%
    dplyr::mutate(scenario = ssp) %>%
    dplyr::select(scenario, region, year, age, sex, unit, value)

  # Compute total pop (sex = Both)
  pop.both <- pop.tot %>%
    dplyr::group_by(scenario, region, year, age, unit) %>%
    dplyr::summarise(value = sum(value),
                     sex = 'Both') %>%
    dplyr::ungroup()

  pop <- dplyr::bind_rows(
    pop.tot,
    pop.both
  )

  invisible(pop)

}

#' calc_pop_ctry_ctry_str
#'
#' Get population data and shares of population for all population segments.
#' To be consistent we make use of the IIASA-WIC Model/scenarios. NUTS3 codes for the European region and ISO3 codes for the ROW.
#' Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth" for this region.
#' @source  https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @keywords socioeconomics, population
#' @return Population and population shares (<5Y; >30Y) for CTRY regions for all years
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @importFrom magrittr %>%
#' @export

calc_pop_ctry_ctry_str<-function(ssp = "SSP2"){


  # Ancillary Functions
  `%!in%` = Negate(`%in%`)

  # First, we read in the population data.
  ssp.data<-raw.ssp.data %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
                  grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT)

  pop<-tibble::as_tibble(ssp.data) %>%
    dplyr::filter(grepl("Population", variable)) %>%
    dplyr::mutate(variable = gsub("-", "and", variable)) %>%
    dplyr::left_join(age_str_mapping, by = 'variable') %>%
    dplyr::filter(complete.cases(age)) %>%
    dplyr::filter(!grepl("Edu", variable)) %>%
    dplyr::mutate(age = dplyr::if_else(age == "95-99" | age == "100", "95+", age)) %>%
    dplyr::mutate(sex = dplyr::case_when(
      stringr::str_detect(variable, "\\|Female\\|") ~ "Female",
      stringr::str_detect(variable, "\\|Male\\|") ~ "Male",
      TRUE ~ "Both"
    )) %>%
    dplyr::select(-variable) %>%
    dplyr::group_by(model, scenario, region, age, sex, unit, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # add NUTS3 regions and aggregate the values to those categories:
    dplyr::rename('ISO3' = 'region') %>%
    dplyr::mutate(ISO3 = dplyr::if_else(ISO3 == 'ROU', 'ROM', ISO3)) %>%  # fix Romania ISO3 code
    dplyr::left_join(dplyr::select(rfasst::ctry_nuts3_codes, ISO3, ISO2) %>%
                       dplyr::distinct(),
                     by = "ISO3") %>%
    dplyr::filter(!is.na(ISO2)) %>%  # rm overseas regions
    dplyr::group_by(model, scenario, region = ISO3, age, sex, year, unit) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  # Taiwan is not included in the database, so we use population projections from OECD Env-Growth.
  # We use China's percentages
  perc.china.str<-pop %>%
    dplyr::filter(region == "CHN") %>%
    # aggregate all NUTS3 to have the values at country level
    dplyr::group_by(model, scenario, region, age, sex, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute the share by age & sex
    dplyr::group_by(model, scenario, region, year) %>%
    dplyr::mutate(pop_tot = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(share = value / pop_tot) %>%
    dplyr::select(model, scenario, age, sex, year, share)

  twn.pop<-tibble::as_tibble(raw.twn.pop) %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
                  grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT) %>%
    dplyr::filter(variable == "Population") %>%
    dplyr::rename(pop_tot = value) %>%
    # add NUTS3 regions and aggregate the values to those categories:
    dplyr::rename('ISO3' = 'region') %>%
    dplyr::left_join(dplyr::select(rfasst::ctry_nuts3_codes, ISO3, ISO2) %>%
                       dplyr::distinct(),
                     by = "ISO3") %>%
    dplyr::filter(!is.na(ISO2)) %>% # rm overseas regions
    dplyr::group_by(model, scenario, region = ISO3, year, unit) %>%
    dplyr::summarise(pop_tot = sum(pop_tot)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-model) %>%
    dplyr::mutate(scenario = ssp) %>%
    gcamdata::repeat_add_columns(tibble::tibble(age = unique(perc.china.str$age))) %>%
    gcamdata::repeat_add_columns(tibble::tibble(sex = unique(perc.china.str$sex))) %>%
    gcamdata::left_join_error_no_match(perc.china.str %>% dplyr::select(-scenario), by = c("year", "age", "sex")) %>%
    dplyr::mutate(value = pop_tot * share,
                  model = "IIASA-WiC POP",
                  scenario = "SSP2_v9_130115") %>%
    dplyr::select(model, scenario, region, age, sex, year, unit, value)

  # We add twn to the database
  pop <-dplyr::bind_rows(pop, twn.pop)

  # Clean the database
  pop.tot <- pop %>%
    dplyr::mutate(scenario = ssp) %>%
    dplyr::select(scenario, region, year, age, sex, unit, value)

  # Compute total pop (sex = Both)
  pop.both <- pop.tot %>%
    dplyr::group_by(scenario, region, year, age, unit) %>%
    dplyr::summarise(value = sum(value),
                     sex = 'Both') %>%
    dplyr::ungroup()

  pop <- dplyr::bind_rows(
    pop.tot,
    pop.both
  )

  invisible(pop)

}



#' calc_gdp_pc_reg
#'
#' Get GDP_pc from the SSP database (SSP_database_v9) for the economic assessment of the health impacts.
#' To be consistent we make use of the IIASA Model/scenarios. rfasst regions considered.
#' @keywords socioeconomics, GDP
#' @source  https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @return GDP_pc for TM5-FASST regions for all years
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @importFrom magrittr %>%
#' @export

calc_gdp_pc_reg<-function(ssp="SSP2"){

  # Ancillary Functions
  `%!in%` = Negate(`%in%`)

  # Get pop data
  pop.all<-get(paste0('pop.all.',ssp))

  # First, we read in the population data.
  ssp.data<-raw.ssp.data %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year=gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
                  grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT)


  gdp<-tibble::as_tibble(raw.gdp) %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100,
           grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT) %>%
    dplyr::filter(variable == "GDP|PPP") %>%
    dplyr::rename(gdp_tot = value) %>%
    #add FASST regions and add the values to those categories:
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>% dplyr::rename(region = subRegionAlt),
                             by = "region") %>%
    dplyr::select(-region) %>%
    dplyr::rename(region = fasst_region) %>%
    dplyr::group_by(model, scenario, region, year, unit) %>%
    dplyr::summarise(gdp_tot = sum(gdp_tot)) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(scenario = ssp)

  # I adjust RUE using the population percentages
  gdp_rus<-gdp %>% dplyr::filter(region == "RUS") %>% dplyr::mutate(gdp_tot = gdp_tot * perc_pop_rus)

  gdp_rue<-gdp %>% dplyr::filter(region == "RUS") %>% dplyr::mutate(region = "RUE") %>% dplyr::mutate(gdp_tot = gdp_tot * perc_pop_rue)

  # We add rus and rue to the database
  gdp_pc<-gdp %>%
    dplyr::filter(region != "RUS") %>%
    dplyr::bind_rows(gdp_rus) %>%
    dplyr::bind_rows(gdp_rue) %>%
    gcamdata::left_join_error_no_match(pop.all %>% dplyr::select(-perc_pop_5, -perc_pop_30), by=c("scenario", "region", "year")) %>%
    dplyr::mutate(gdp_pc = (gdp_tot * CONV_BIL) / (pop_tot * CONV_MIL)) %>%
    dplyr::select(-model, -unit.x, -unit.y, -gdp_tot, -pop_tot) %>%
    dplyr::mutate(unit = "2005$/pers")

  invisible(gdp_pc)

}


#' calc_gdp_pc_ctry_nuts3
#'
#' Get GDP_pc from the SSP database (SSP_database_v9) for the economic assessment of the health impacts.
#' To be consistent we make use of the IIASA-WIC Model/scenarios. NUTS3 codes for the European region and ISO3 codes for the ROW.
#' @keywords socioeconomics, GDP
#' @source  https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @return GDP_pc for TM5-FASST regions for all years
#' @param ssp Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2
#' @importFrom magrittr %>%
#' @export

calc_gdp_pc_ctry_nuts3<-function(ssp="SSP2"){

  # Ancillary Functions
  `%!in%` = Negate(`%in%`)

  # Get pop data
  pop.all<-get(paste0('pop.all.ctry_nuts3.str.',ssp)) %>%
    dplyr::filter(sex == 'Both') %>%
    dplyr::group_by(NUTS3 = region, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  gdp<-tibble::as_tibble(raw.gdp) %>%
    tidyr::gather(year, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::filter(year >= 2010, year <= 2100, grepl(ssp, SCENARIO)) %>%
    dplyr::rename(model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT) %>%
    dplyr::filter(variable == "GDP|PPP") %>%
    dplyr::rename(gdp_tot = value) %>%
    # add NUTS3 regions and aggregate the values to those categories:
    dplyr::rename('ISO3' = 'region') %>%
    dplyr::left_join(rfasst::ctry_nuts3_codes,
                     by = "ISO3", relationship = "many-to-many") %>%
    dplyr::select(-ISO3, -ISO2) %>%
    dplyr::filter(!is.na(NUTS3)) %>% # rm overseas regions
    # add pop-weights to downscale to NUTS3 level
    dplyr::left_join(weight.nuts.pop.nuts3 %>%
                       tibble::as_tibble() %>%
                       dplyr::rename(NUTS3 = geo),
                     by = c('NUTS3'), relationship = "many-to-many") %>%
    dplyr::mutate(weight = dplyr::if_else(is.na(weight) & nchar(NUTS3) == 5, 0, weight)) %>% # if NUTS3 but empty weight
    dplyr::mutate(gdp_tot = dplyr::if_else(!is.na(weight), gdp_tot * weight, gdp_tot)) %>%
    dplyr::select(model, scenario, variable, unit, year, gdp_tot, NUTS3)

  gdp_pc <- gdp %>%
    dplyr::distinct() %>%
    gcamdata::left_join_error_no_match(pop.all,
                                       by=c("NUTS3", "year")) %>%
    dplyr::mutate(gdp_pc = (gdp_tot * CONV_BIL) / (value * CONV_MIL)) %>%
    dplyr::select(-model, -gdp_tot, -value) %>%
    dplyr::mutate(unit = "2005$/pers")

  invisible(gdp_pc)

}



#' interpLinear
#'
#' Function to interpolate annual values using decade-averages
#' @keywords interpolate
#' @param d Data frame to be interpolated
#' @param y_start Starting year (start of the decade)
#' @param y_end End year (End of the decade)
#' @export

interpLinear <- function(d, y_start, y_end){
  n_value <- (y_end - y_start)
  n_iterations <- c(1:n_value)
  x_start <- paste0("X", y_start)
  x_end <- paste0("X", y_end)
  for(n_count in n_iterations){
    y_n <- y_start + n_count
    x_new <- paste0("X", y_n)
    d[[x_new]] <- (d[[x_start]] + (d[[x_end]] - d[[x_start]]) * (n_count / n_value))
  }
  return(d)
}


