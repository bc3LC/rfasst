#=========================================================
# Ancillary data
#=========================================================

#' Selected pollutants
#'
#' @description Selected pollutants
#' @source -
#' @format vector
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::selected_pollutants
#' }
"selected_pollutants"


#' SSP data
#'
#' @description Country level population and GDP data per SSP (SSP_database_v9.csv). To be consistent we make use of the IIASA-WIC Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.ssp.data
#' }
"raw.ssp.data"


#'Socioeconomics Taiwan
#'
#' @description Population and GDP data per SSP (SSP_database_v9.csv). Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth"
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.twn.pop
#' }
"raw.twn.pop"


#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.gdp
#' }
"raw.gdp"


#' GDP-SSP database
#'
#' @description Population data from Eurostat 2021 by NUTS0, NUTS1, NUTS2, and NUTS3.
#' @source https://doi.org/10.2908/CENS_21AGR3
#' @format .tsv.gz
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.nuts.pop
#' }
"raw.nuts.pop"

#' GDP-SSP database
#'
#' @description NUTS3 population weights by each European country based on Eurostat data 2021.
#' @source https://doi.org/10.2908/CENS_21AGR3
#' @format .tsv.gz
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::weight.nuts.pop
#' }
"weight.nuts.pop"

#' GDP-SSP database
#'
#' @description NUTS3 population weights by each European country based on Eurostat data 2021 over F, M and T sex.
#' @source https://doi.org/10.2908/CENS_21AGR3
#' @format .tsv.gz
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::weight.nuts.pop.sex
#' }
"weight.nuts.pop.sex"

#' GDP-SSP database
#'
#' @description NUTS3 population weight mean by age among all European countries based on Eurostat data 2021 over F, M and T sex.
#' @source https://doi.org/10.2908/CENS_21AGR3
#' @format .tsv.gz
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::weight.nuts.pop.nuts3
#' }
"weight.nuts.pop.nuts3"

#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP1 (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_pc.SSP1
#' }
"gdp_pc.SSP1"

#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP2 (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_pc.SSP2
#' }
"gdp_pc.SSP2"

#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP3 (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_pc.SSP3
#' }
"gdp_pc.SSP3"

#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP4 (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_pc.SSP4
#' }
"gdp_pc.SSP4"

#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP5 (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_pc.SSP5
#' }
"gdp_pc.SSP5"

#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP4 (SSP_database_v9.csv).
#' To be consistent we make use of the IIASA Model/scenarios. Data aggregated at NUTS3 level when possible. Otherwise at ISO3 level.
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_pc.ctry_nuts3.SSP1
#' }
"gdp_pc.ctry_nuts3.SSP1"

#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP4 (SSP_database_v9.csv).
#' To be consistent we make use of the IIASA Model/scenarios. Data aggregated at NUTS3 level when possible. Otherwise at ISO3 level.
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_pc.ctry_nuts3.SSP2
#' }
"gdp_pc.ctry_nuts3.SSP2"

#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP4 (SSP_database_v9.csv).
#' To be consistent we make use of the IIASA Model/scenarios. Data aggregated at NUTS3 level when possible. Otherwise at ISO3 level.
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_pc.ctry_nuts3.SSP3
#' }
"gdp_pc.ctry_nuts3.SSP3"

#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP4 (SSP_database_v9.csv).
#' To be consistent we make use of the IIASA Model/scenarios. Data aggregated at NUTS3 level when possible. Otherwise at ISO3 level.
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_pc.ctry_nuts3.SSP4
#' }
"gdp_pc.ctry_nuts3.SSP4"

#'GDP-SSP database
#'
#' @description Filtered GDP data per SSP5 (SSP_database_v9.csv).
#' To be consistent we make use of the IIASA Model/scenarios. Data aggregated at NUTS3 level when possible. Otherwise at ISO3 level.
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_pc.ctry_nuts3.SSP5
#' }
"gdp_pc.ctry_nuts3.SSP5"

#'GDP-SSP database
#'
#' @description GDP growth rates. To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_growth.SSP1
#' }
"gdp_growth.SSP1"

#'GDP-SSP database
#'
#' @description GDP growth rates. To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_growth.SSP2
#' }
"gdp_growth.SSP2"

#'GDP-SSP database
#'
#' @description GDP growth rates. To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_growth.SSP3
#' }
"gdp_growth.SSP3"

#'GDP-SSP database
#'
#' @description GDP growth rates. To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_growth.SSP4
#' }
"gdp_growth.SSP4"

#'GDP-SSP database
#'
#' @description GDP growth rates. To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_growth.SSP5
#' }
"gdp_growth.SSP5"



#'GDP-SSP database
#'
#' @description Filtered Population data per SSP1 (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.SSP1
#' }
"pop.all.SSP1"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP2 (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.SSP2
#' }
"pop.all.SSP2"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP3 (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.SSP3
#' }
"pop.all.SSP3"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP4 (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.SSP4
#' }
"pop.all.SSP4"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv). To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.SSP5
#' }
"pop.all.SSP5"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP1 (SSP_database_v9.csv) by age and sex. To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.str.SSP1
#' }
"pop.all.str.SSP1"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP2 (SSP_database_v9.csv) by age and sex. To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.SSP2
#' }
"pop.all.str.SSP2"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP3 (SSP_database_v9.csv) by age and sex. To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.str.SSP3
#' }
"pop.all.str.SSP3"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP4 (SSP_database_v9.csv) by age and sex. To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.str.SSP4
#' }
"pop.all.str.SSP4"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex. To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.str.SSP5
#' }
"pop.all.str.SSP5"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex aggregated at NUTS3 level when possible. Otherwise at ISO3 level.
#' To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.ctry_nuts3.str.SSP1
#' }
"pop.all.ctry_nuts3.str.SSP1"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex aggregated at NUTS3 level when possible. Otherwise at ISO3 level.
#' To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.ctry_nuts3.str.SSP2
#' }
"pop.all.ctry_nuts3.str.SSP2"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex aggregated at NUTS3 level when possible. Otherwise at ISO3 level.
#' To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.ctry_nuts3.str.SSP3
#' }
"pop.all.ctry_nuts3.str.SSP3"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex aggregated at NUTS3 level when possible. Otherwise at ISO3 level.
#' To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.ctry_nuts3.str.SSP4
#' }
"pop.all.ctry_nuts3.str.SSP4"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex aggregated at NUTS3 level when possible. Otherwise at ISO3 level.
#' To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.ctry_nuts3.str.SSP5
#' }
"pop.all.ctry_nuts3.str.SSP5"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex aggregated at CTRY level.
#' To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.ctry_ctry.str.SSP1
#' }
"pop.all.ctry_ctry.str.SSP1"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex aggregated at CTRY level.
#' To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.ctry_ctry.str.SSP2
#' }
"pop.all.ctry_ctry.str.SSP2"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex aggregated at CTRY level.
#' To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.ctry_ctry.str.SSP3
#' }
"pop.all.ctry_ctry.str.SSP3"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex aggregated at CTRY level.
#' To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.ctry_ctry.str.SSP4
#' }
"pop.all.ctry_ctry.str.SSP4"

#'GDP-SSP database
#'
#' @description Filtered Population data per SSP5 (SSP_database_v9.csv) by age and sex aggregated at CTRY level.
#' To be consistent we make use of the IIASA Model/scenarios
#' @source https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::pop.all.ctry_ctry.str.SSP5
#' }
"pop.all.ctry_ctry.str.SSP5"

#'ctry_nuts3_codes
#'
#' @description A mapping table that links ISO2, ISO3, and NUTS3 codes. For regions outside Europe, the NUTS3 code corresponds to the ISO3 code.
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::ctry_nuts3_codes
#' }
"ctry_nuts3_codes"

#=========================================================
# Module 2
#=========================================================
#'Base concentration
#'
#' @description Fine Particulate Matter (PM2.5), and Ozone (O3 and M6M) concentration levels in the base year (2000) per TM5-FASST region
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.base_conc
#' }
"raw.base_conc"

#'Base Nat PM2.5 concentration
#'
#' @description Natural Fine Particulate Matter (PM2.5) concentration levels in the base year (2000) per TM5-FASST region
#' @source Results from CAMS
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.base_conc.nat
#' }
"raw.base_conc.nat"


#'Base emissions
#'
#' @description Emissions in the base year (2000) per TM5-FASST region of the main precursors for Particulate Matter (PM2.5), and Ozone (O3 and M6M) formation. These include Black Carbon (BC), Carbon Dioxide (CO2), Methane (CH4), Nitrogen Dioxide (N2O), Organic Matter (POM), Nitrogen Oxides (NOx), sulphur dioxide (SO2), ammonia (NH3) and non-methane volatile organic compounds (NMVOC or VOC)
#' @source RCP database: https://tntcat.iiasa.ac.at/RcpDb/dsd?Action=htmlpage&page=welcome
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.base_em
#' }
"raw.base_em"

#'Base AOT40 conc
#'
#' @description O3 concentration above a threshold of 40 ppbV (AOT40), in the base year (2000) per TM5-FASST region
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.base_aot
#' }
"raw.base_aot"

#'Base Mi conc
#'
#' @description Seasonal mean daytime O3 concentration (M7 for the 7-hour mean and M12 for the 12-hour mean), in the base year (2000) per TM5-FASST region
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.base_mi
#' }
"raw.base_mi"

#------------------------------------------------------------
# COUNTRY-NUTS sf
#------------------------------------------------------------
#' ctry_nuts_sf
#'
#' @description sf object for the entire World containing the NUTS3 region code if avilable (Europe) or the country ISO3 code otherwise
#' @source Processing of territorial-units-statistics. Details in saveDataFiles.R
#' @format sf
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::ctry_nuts_sf
#' }
"ctry_nuts_sf"

#' nuts_sf
#'
#' @description sf object for Europe containing the NUTS3 region code
#' @source Processing of territorial-units-statistics. Details in saveDataFiles.R
#' @format sf
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::nuts_sf
#' }
"nuts_sf"

#' nuts_europe_sf
#'
#' @description sf object for Europe containing the NUTS3 region code of European continent regions (no overseas)
#' @source Processing of territorial-units-statistics. Details in saveDataFiles.R
#' @format sf
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::nuts_europe_sf
#' }
"nuts_europe_sf"

#------------------------------------------------------------
# PM2.5
#------------------------------------------------------------
#' SRC BC
#'
#' @description Source-receptor coefficients (SRC) between BC emissions and BC concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.bc
#' }
"src.bc"

#' SRC POM
#'
#' @description Source-receptor coefficients (SRC) between POM emissions and POM concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.pom
#' }
"src.pom"

#' Urban Increment
#'
#' @description Adjustment factor for primary PM2.5 concentrations (BC and POM), which are assumed to be concentrated in urban areas.
#' @source Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.urb_incr
#' }
"raw.urb_incr"

#------------------------------------------------------------
# NO3

#' no3_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and NO3 (PM2.5) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.no3_nox
#' }
"src.no3_nox"


#' no3_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and NO3 (PM2.5) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.no3_so2
#' }
"src.no3_so2"


#' no3_nh3
#'
#' @description Source-receptor coefficients (SRC) between NH3 emissions and NO3 (PM2.5) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.no3_nh3
#' }
"src.no3_nh3"
#------------------------------------------------------------
# SO4

#' so4_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and SO4 (PM2.5) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.so4_nox
#' }
"src.so4_nox"


#' so4_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and SO4 (PM2.5) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.so4_so2
#' }
"src.so4_so2"


#' so4_nh3
#'
##' @description Source-receptor coefficients (SRC) between NH3 emissions and SO4 (PM2.5) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.so4_nh3
#' }
"src.so4_nh3"

#------------------------------------------------------------
# NH4

#' nh4_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and NH4 (PM2.5) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.nh4_nox
#' }
"src.nh4_nox"

#' nh4_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and NH4 (PM2.5) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.nh4_so2
#' }
"src.nh4_so2"

#' nh4_nh3
#'
#' @description Source-receptor coefficients (SRC) between NH3 emissions and NH4 (PM2.5) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.nh4_nh3
#' }
"src.nh4_nh3"

#------------------------------------------------------------
# O3
#------------------------------------------------------------

#' src.o3_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and O3 concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.o3_nox
#' }
"src.o3_nox"

#' src.o3_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and O3 concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.o3_so2
#' }
"src.o3_so2"

#' src.o3_nmvoc
#'
#' @description Source-receptor coefficients (SRC) between NMVOC emissions and O3 concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.o3_nmvoc
#' }
"src.o3_nmvoc"

#' src.o3_ch4
#'
#' @description Source-receptor coefficients (SRC) between CH4 emissions and O3 concentration level, normalized from existing literature (not pre-computed with TM5-FASST)
#' @source Fiore, A.M., West, J.J., Horowitz, L.W., Naik, V. and Schwarzkopf, M.D., 2008. Characterizing the tropospheric ozone response to methane emission controls and the benefits to climate and air quality. Journal of Geophysical Research: Atmospheres, 113(D8).
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.o3_ch4
#' }
"src.o3_ch4"

#------------------------------------------------------------
# M6M
#------------------------------------------------------------

#' src.m6m_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and M6M (O3) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.m6m_nox
#' }
"src.m6m_nox"

#' src.m6m_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and M6M (O3) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.m6m_so2
#' }
"src.m6m_so2"

#' src.m6m_nmvoc
#'
#' @description Source-receptor coefficients (SRC) between NMVOC emissions and M6M (O3) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.m6m_nmvoc
#' }
"src.m6m_nmvoc"

#' src.m6m_ch4
#'
#' @description Source-receptor coefficients (SRC) between CH4 emissions and M6M (O3) concentration level, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.m6m_ch4
#' }
"src.m6m_ch4"

#------------------------------------------------------------
# AOT40
#------------------------------------------------------------
# Maize

#' src.maize_aot40_ch4
#'
#' @description Source-receptor coefficients (SRC) between CH4 emissions and AOT40 (O3) concentration level for maize, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.maize_aot40_ch4
#' }
"src.maize_aot40_ch4"

#' src.maize_aot40_nmvoc
#'
#' @description Source-receptor coefficients (SRC) between NMVOC emissions and AOT40 (O3) concentration level for maize, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.maize_aot40_nmvoc
#' }
"src.maize_aot40_nmvoc"

#' src.maize_aot40_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and AOT40 (O3) concentration level for maize, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.maize_aot40_nox
#' }
"src.maize_aot40_nox"

#' src.maize_aot40_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and AOT40 (O3) concentration level for maize, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.maize_aot40_so2
#' }
"src.maize_aot40_so2"

#------------------------------------------------------------
# Rice

#' src.rice_aot40_ch4
#'
#' @description Source-receptor coefficients (SRC) between CH4 emissions and AOT40 (O3) concentration level for rice, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.rice_aot40_ch4
#' }
"src.rice_aot40_ch4"

#' src.rice_aot40_nmvoc
#'
#' @description Source-receptor coefficients (SRC) between NMVOC emissions and AOT40 (O3) concentration level for rice, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.rice_aot40_nmvoc
#' }
"src.rice_aot40_nmvoc"

#' src.rice_aot40_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and AOT40 (O3) concentration level for rice, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.rice_aot40_nox
#' }
"src.rice_aot40_nox"

#' src.rice_aot40_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and AOT40 (O3) concentration level for rice, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.rice_aot40_so2
#' }
"src.rice_aot40_so2"

#------------------------------------------------------------
# Soybeans

#' src.soy_aot40_ch4
#'
#' @description Source-receptor coefficients (SRC) between CH4 emissions and AOT40 (O3) concentration level for soy, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.soy_aot40_ch4
#' }
"src.soy_aot40_ch4"

#' src.soy_aot40_nmvoc
#'
#' @description Source-receptor coefficients (SRC) between NMVOC emissions and AOT40 (O3) concentration level for soy, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.soy_aot40_nmvoc
#' }
"src.soy_aot40_nmvoc"

#' src.soy_aot40_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and AOT40 (O3) concentration level for soy, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.soy_aot40_nox
#' }
"src.soy_aot40_nox"

#' src.soy_aot40_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and AOT40 (O3) concentration level for soy, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.soy_aot40_so2
#' }
"src.soy_aot40_so2"

#------------------------------------------------------------
# Wheat

#' src.wheat_aot40_ch4
#'
#' @description Source-receptor coefficients (SRC) between CH4 emissions and AOT40 (O3) concentration level for wheat, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.wheat_aot40_ch4
#' }
"src.wheat_aot40_ch4"

#' src.wheat_aot40_nmvoc
#'
#' @description Source-receptor coefficients (SRC) between NMVOC emissions and AOT40 (O3) concentration level for wheat, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.wheat_aot40_nmvoc
#' }
"src.wheat_aot40_nmvoc"

#' src.wheat_aot40_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and AOT40 (O3) concentration level for wheat, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.wheat_aot40_nox
#' }
"src.wheat_aot40_nox"

#' src.wheat_aot40_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and AOT40 (O3) concentration level for wheat, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.wheat_aot40_so2
#' }
"src.wheat_aot40_so2"

#------------------------------------------------------------
# Mi
#------------------------------------------------------------
# Maize

#' src.maize_mi_ch4
#'
#' @description Source-receptor coefficients (SRC) between CH4 emissions and Mi (O3) concentration level for maize, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.maize_mi_ch4
#' }
"src.maize_mi_ch4"

#' src.maize_mi_nmvoc
#'
#' @description Source-receptor coefficients (SRC) between NMVOC emissions and Mi (O3) concentration level for maize, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.maize_mi_nmvoc
#' }
"src.maize_mi_nmvoc"

#' src.maize_mi_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and Mi (O3) concentration level for maize, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.maize_mi_nox
#' }
"src.maize_mi_nox"

#' src.maize_mi_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and Mi (O3) concentration level for maize, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.maize_mi_so2
#' }
"src.maize_mi_so2"

#------------------------------------------------------------
# Rice

#' src.rice_mi_ch4
#'
#' @description Source-receptor coefficients (SRC) between CH4 emissions and Mi (O3) concentration level for rice, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.rice_mi_ch4
#' }
"src.rice_mi_ch4"

#' src.rice_mi_nmvoc
#'
#' @description Source-receptor coefficients (SRC) between NMVOC emissions and Mi (O3) concentration level for rice, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.rice_mi_nmvoc
#' }
"src.rice_mi_nmvoc"

#' src.rice_mi_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and Mi (O3) concentration level for rice, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.rice_mi_nox
#' }
"src.rice_mi_nox"

#' src.rice_mi_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and Mi (O3) concentration level for rice, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.rice_mi_so2
#' }
"src.rice_mi_so2"

#------------------------------------------------------------
# Soybeans

#' src.soy_mi_ch4
#'
#' @description Source-receptor coefficients (SRC) between CH4 emissions and Mi (O3) concentration level for soy, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.soy_mi_ch4
#' }
"src.soy_mi_ch4"

#' src.soy_mi_nmvoc
#'
#' @description Source-receptor coefficients (SRC) between NMVOC emissions and Mi (O3) concentration level for soy, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.soy_mi_nmvoc
#' }
"src.soy_mi_nmvoc"

#' src.soy_mi_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and Mi (O3) concentration level for soy, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.soy_mi_nox
#' }
"src.soy_mi_nox"

#' src.soy_mi_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and Mi (O3) concentration level for soy, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.soy_mi_so2
#' }
"src.soy_mi_so2"

#----------------------------------------------------------------------
# Wheat

#' src.wheat_mi_ch4
#'
#' @description Source-receptor coefficients (SRC) between CH4 emissions and Mi (O3) concentration level for wheat, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.wheat_mi_ch4
#' }
"src.wheat_mi_ch4"

#' src.wheat_mi_nmvoc
#'
#' @description Source-receptor coefficients (SRC) between NMVOC emissions and Mi (O3) concentration level for wheat, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.wheat_mi_nmvoc
#' }
"src.wheat_mi_nmvoc"

#' src.wheat_mi_nox
#'
#' @description Source-receptor coefficients (SRC) between NOx emissions and Mi (O3) concentration level for wheat, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.wheat_mi_nox
#' }
"src.wheat_mi_nox"

#' src.wheat_mi_so2
#'
#' @description Source-receptor coefficients (SRC) between SO2 emissions and Mi (O3) concentration level for wheat, pre-computed from a 20% emission reduction of component i  in region xk relative to the base run
#' @source Results from TM5
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::src.wheat_mi_so2
#' }
"src.wheat_mi_so2"

#=========================================================
# Module 3
#=========================================================
#' raw.mort.rates
#'
#' @description cause-specific baseline mortalities from stroke, ischemic heart disease (IHD), chronic obstructive pulmonary disease (COPD), acute lower respiratory illness diseases (ALRI), lung cancer (LC), and diabetes mellitus type II (dm).
#' @source TM5-FASST, v4.7 -- Global Burden of Disease
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.mort.rates
#' }
"raw.mort.rates"

#' raw.mort.rates.plus
#'
#' @description cause-specific baseline mortalities from stroke, ischemic heart disease (IHD), chronic obstructive pulmonary disease (COPD), acute lower respiratory illness diseases (ALRI), lung cancer (LC), and diabetes mellitus type II (dm) by age and sex.
#' @source TM5-FASST, Global Burden of Disease Study 2021 (GBD 2021) Results; Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022.; Available from https://vizhub.healthdata.org/gbd-results/.
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.mort.rates.plus
#' }
"raw.mort.rates.plus"

#' raw.mort.rates.ctry_nuts3
#'
#' @description cause-specific baseline mortalities from stroke, ischemic heart disease (IHD), chronic obstructive pulmonary disease (COPD), acute lower respiratory illness diseases (ALRI), lung cancer (LC), and diabetes mellitus type II (dm) by age and sex and NUTS3 when possible, CTRY otherwise.
#' @source TM5-FASST, Global Burden of Disease Study 2021 (GBD 2021) Results; Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022.; Available from https://vizhub.healthdata.org/gbd-results/.
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.mort.rates.ctry_nuts3
#' }
"raw.mort.rates.ctry_nuts3"

#' raw.mort.rates.ctry_ctry
#'
#' @description cause-specific baseline mortalities from stroke, ischemic heart disease (IHD), chronic obstructive pulmonary disease (COPD), acute lower respiratory illness diseases (ALRI), lung cancer (LC), and diabetes mellitus type II (dm) by age and sex and CTRY.
#' @source TM5-FASST, Global Burden of Disease Study 2021 (GBD 2021) Results; Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022.; Available from https://vizhub.healthdata.org/gbd-results/.
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.mort.rates.ctry_ctry
#' }
"raw.mort.rates.ctry_ctry"

#' raw.rr.fusion
#'
#' @description  Relative risk of death attributable to a change in population-weighted mean pollutant concentration using the FUSION model. From Burnett, R.T., Spadaro, J.V., Garcia, G.R. and Pope, C.A., 2022. Designing health impact functions to assess marginal changes in outdoor fine particulate matter. Environmental Research, 204, p.112245.
#' @source Lim, S.S., Vos, T., Flaxman, A.D., Danaei, G., Shibuya, K., Adair-Rohani, H., AlMazroa, M.A., Amann, M., Anderson, H.R., Andrews, K.G. and Aryee, M., 2012. A comparative risk assessment of burden of disease and injury attributable to 67 risk factors and risk factor clusters in 21 regions, 1990–2010: a systematic analysis for the Global Burden of Disease Study 2010. The lancet, 380(9859), pp.2224-2260.
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.rr.fusion
#' }
"raw.rr.fusion"

#' raw.rr.gemm.param
#'
#' @description  Parameters for the estimation of the RR of death attributable to a change in population-weighted mean pollutant concentration, with the extreme Chinese cohort, based on the GEMM model from Burnett et al 2018.
#' @source Burnett, R., Chen, H., Szyszkowicz, M., Fann, N., Hubbell, B., Pope Iii, C.A., Apte, J.S., Brauer, M., Cohen, A., Weichenthal, S. and Coggins, J., 2018. Global estimates of mortality associated with long-term exposure to outdoor fine particulate matter. Proceedings of the National Academy of Sciences, 115(38), pp.9592-9597.
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.rr.gemm.param
#' }
"raw.rr.gemm.param"



#' raw.rr.gbd.param
#'
#' @description  Parameters for the estimation of the RR of death attributable to a change in population-weighted mean pollutant concentration, based on the GBD 2018 study.
#' @source GBD 2018
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.rr.gbd.param
#' }
"raw.rr.gbd.param"

#' raw.daly
#'
#' @description  Data on Disability Adjusted Life Years (DALYs). Used the latest available data (2019)
#' @source Institute for Health Metrics and Evaluation (http://www.healthdata.org/)
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.daly
#' }
"raw.daly"

#' raw.yll.pm25
#'
#' @description  Years of Life Lost (YLLs) to Mortality ratios attributable to PM2.5 exposure
#' @source TM5-FASST
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.yll.pm25
#' }
"raw.yll.pm25"

#' raw.yll.o3
#'
#' @description  Years of Life Lost (YLLs) to Mortality ratios attributable to O3 exposure
#' @source TM5-FASST
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::raw.yll.o3
#' }
"raw.yll.o3"

#=========================================================
# Mapping
#=========================================================
#' fasst_reg
#'
#' @description  Mapping of countries to TM5-FASST regions
#' @source TM5-FASST
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::fasst_reg
#' }
"fasst_reg"

#' GCAM_reg
#'
#' @description  Mapping of countries to GCAM regions
#' @source GCAM
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::GCAM_reg
#' }
"GCAM_reg"

#' GCAM_reg_EUR
#'
#' @description  Mapping of countries to GCAM-Europe regions
#' @source GCAM
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::GCAM_reg_EUR
#' }
"GCAM_reg_EUR"

#' country_iso
#'
#' @description  Mapping of countries to iso3 codes
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::country_iso
#' }
"country_iso"

#' adj_rus
#'
#' @description  Shares to distribute emissions of different species between Russia Eastern (RUE) and Western (RUS)
#' @source TM5-FASST
#' @examples
#' \dontrun{
#'  library(rfasst);
#'  rfasst::adj_rus
#' }
"adj_rus"
#'
#'
#' Percen
#'
#' @description  Percentages to downscale GCAM emissions to country-level
#' @source For GHGs: European Commission, Joint Research Centre (EC-JRC)/Netherlands Environmental Assessment Agency (PBL) (EDGARv7.0_GHG website (https://edgar.jrc.ec.europa.eu/dataset_ghg70)).
#' Emissions Database for Global Atmospheric Research (EDGAR), release EDGAR v7.0_GHG (1970 - 2021) of September 2022. For the energy related sectors the activity data are mainly based on IEA data from IEA (2021) World Energy Balances, www.iea.org/statistics,
#' Crippa, M., Guizzardi, D., Banja, M., Solazzo, E., Muntean, M., Schaaf, E., Pagani, F., Monforti-Ferrario, F., Olivier, J., Quadrelli, R., Risquez Martin, A., Taghavi-Moharamli, P., Grassi, G., Rossi, S., Jacome Felix Oom, D., Branco, A., San-Miguel-Ayanz, J. and Vignati, E., CO2 emissions of all world countries - 2022 Report, EUR 31182 EN, Publications Office of the European Union, Luxembourg, 2022, doi:10.2760/730164, JRC130363
#' Crippa, M., Guizzardi, D., Solazzo, E., Muntean, M., Schaaf, E., Monforti-Ferrario, F., Banja, M., Olivier, J.G.J., Grassi, G., Rossi, S., Vignati, E.,GHG emissions of all world countries - 2021 Report, EUR 30831 EN, Publications Office of the European Union, Luxembourg, 2021, ISBN 978-92-76-41547-3, doi:10.2760/173513, JRC126363
#' For APs: https://zenodo.org/record/3754964#.Y3O_lXbMKUk (CEDS-GBD)
#'
#'
#' \dontrun{
#'  library(rfasst);
#'  rfasst::Percen
#' }
#'
#'
"Percen"

#' Percen_EUR
#'
#' @description  Percentages to downscale GCAM-Europe emissions to country-level
#' @source For GHGs: European Commission, Joint Research Centre (EC-JRC)/Netherlands Environmental Assessment Agency (PBL) (EDGARv7.0_GHG website (https://edgar.jrc.ec.europa.eu/dataset_ghg70)).
#' Emissions Database for Global Atmospheric Research (EDGAR), release EDGAR v7.0_GHG (1970 - 2021) of September 2022. For the energy related sectors the activity data are mainly based on IEA data from IEA (2021) World Energy Balances, www.iea.org/statistics,
#' Crippa, M., Guizzardi, D., Banja, M., Solazzo, E., Muntean, M., Schaaf, E., Pagani, F., Monforti-Ferrario, F., Olivier, J., Quadrelli, R., Risquez Martin, A., Taghavi-Moharamli, P., Grassi, G., Rossi, S., Jacome Felix Oom, D., Branco, A., San-Miguel-Ayanz, J. and Vignati, E., CO2 emissions of all world countries - 2022 Report, EUR 31182 EN, Publications Office of the European Union, Luxembourg, 2022, doi:10.2760/730164, JRC130363
#' Crippa, M., Guizzardi, D., Solazzo, E., Muntean, M., Schaaf, E., Monforti-Ferrario, F., Banja, M., Olivier, J.G.J., Grassi, G., Rossi, S., Vignati, E.,GHG emissions of all world countries - 2021 Report, EUR 30831 EN, Publications Office of the European Union, Luxembourg, 2021, ISBN 978-92-76-41547-3, doi:10.2760/173513, JRC126363
#' For APs: https://zenodo.org/record/3754964#.Y3O_lXbMKUk (CEDS-GBD)
#'
#'
#' \dontrun{
#'  library(rfasst);
#'  rfasst::Percen_EUR
#' }
#'
#'
"Percen_EUR"

#' #' my_pol
#'
#' @description  Information about GCAM and TM5-FASST regions and pollutants and their equivalences.
#' @source GCAM
#' \dontrun{
#'  library(rfasst);
#'  rfasst::my_pol
#' }
#'
#'
"my_pol"

#' d.iso
#'
#' @description  Countries to GCAM regions
#' \dontrun{
#'  library(rfasst);
#'  rfasst::d.iso
#' }
#'
#'
"d.iso"

#' d.iso_EUR
#'
#' @description  Countries to GCAM-Europe regions
#' \dontrun{
#'  library(rfasst);
#'  rfasst::d.iso_EUR
#' }
#'
#'
"d.iso_EUR"

#' d.gcam.commod.o3
#'
#' @description  O3 to GCAM commodities (based on their carbon fixation pathways; C3 and C4 categories)
#' @source Own assumptions
#' \dontrun{
#'  library(rfasst);
#'  rfasst::d.gcam.commod.o3
#' }
#'
#'
"d.gcam.commod.o3"

#' d.ha
#'
#' @description  Harvested area by crop for weights to map O3 crops to GCAM commodities
#' @source GFDL-NOAA
#' \dontrun{
#'  library(rfasst);
#'  rfasst::d.ha
#' }
#'
#'
"d.ha"

#' Regions
#'
#' @description  Combined GCAM regions
#' \dontrun{
#'  library(rfasst);
#'  rfasst::Regions
#' }
#'
#'
"Regions"

#' Regions_EUR
#'
#' @description  Combined GCAM-Europe regions
#' \dontrun{
#'  library(rfasst);
#'  rfasst::Regions_EUR
#' }
#'
#'
"Regions_EUR"

#' d.weight.gcam
#'
#' @description  O3 to GCAM commodities (based on their carbon fixation pathways; C3 and C4 categories)
#' @source Own assumptions
#' \dontrun{
#'  library(rfasst);
#'  rfasst::d.weight.gcam
#' }
#'
#'
"d.weight.gcam"

#' d.weight.gcam_EUR
#'
#' @description  O3 to GCAM-Europe commodities (based on their carbon fixation pathways; C3 and C4 categories)
#' @source Own assumptions
#' \dontrun{
#'  library(rfasst);
#'  rfasst::d.weight.gcam_EUR
#' }
#'
#'
"d.weight.gcam_EUR"

#=========================================================
# Constants
#=========================================================
#' all_years
#'
#' @description  Years to be analyzed: c('2005','2010','2020','2030','2040','2050','2060','2070','2080','2090','2100')
#' \dontrun{
#'  library(rfasst);
#'  rfasst::all_years
#' }
#'
#'
"all_years"

#' ch4_htap_pert
#'
#' @description  Normalized CH4-O3 relation from Fiore et al (2008)
#' @source Fiore, A.M., West, J.J., Horowitz, L.W., Naik, V. and Schwarzkopf, M.D., 2008. Characterizing the tropospheric ozone response to methane emission controls and the benefits to climate and air quality. Journal of Geophysical Research: Atmospheres, 113(D8).
#' \dontrun{
#'  library(rfasst);
#'  rfasst::ch4_htap_pert
#' }
#'
#'
"ch4_htap_pert"

#' CROP_ANALYSIS
#'
#' @description  Crops that are included in the analysis
#' \dontrun{
#'  library(rfasst);
#'  rfasst::CROP_ANALYSIS
#' }
#'
#'
"CROP_ANALYSIS"

#' perc_pop_rus
#'
#' @description  Percentages to divide population between Russia and Russia Eastern
#' \dontrun{
#'  library(rfasst);
#'  rfasst::perc_pop_rus
#' }
#'
#'
"perc_pop_rus"

#' perc_pop_rue
#'
#' @description  Percentages to divide population between Russia and Russia Eastern
#' \dontrun{
#'  library(rfasst);
#'  rfasst::perc_pop_rue
#' }
#'
#'
"perc_pop_rue"

#' map_pol
#'
#' @description  Indicate the pollutants whose emissions are mapped (if map=T in m1_emissions_rescale)
#' \dontrun{
#'  library(rfasst);
#'  rfasst::map_pol
#' }
#'
#'
"map_pol"

#' gdp_eu_2005
#'
#' @description  Base GDP for EU in 2005
#' @source OECD
#' \dontrun{
#'  library(rfasst);
#'  rfasst::gdp_eu_2005
#' }
#'
#'
"gdp_eu_2005"

#' vsl_eu_2005_lb
#'
#' @description  Lower bound for the Value of Statistical Life (VSL)
#' @source OECD
#' \dontrun{
#'  library(rfasst);
#'  rfasst::vsl_eu_2005_lb
#' }
#'
#'
"vsl_eu_2005_lb"


#' vsl_eu_2005_ub
#'
#' @description  Upper bound for the Value of Statistical Life (VSL)
#' @source OECD
#' \dontrun{
#'  library(rfasst);
#'  rfasst::vsl_eu_2005_lb
#' }
#'
#'
"vsl_eu_2005_ub"

#' vsl_eu_2005
#'
#' @description  Median bound for the Value of Statistical Life (VSL)
#' @source OECD
#' \dontrun{
#'  library(rfasst);
#'  rfasst::vsl_eu_2005
#' }
#'
#'
"vsl_eu_2005"

#' beta_dongetal2021
#'
#' @description  Beta for damages in GDP growth based on Dong et al 2021
#' @source OECD
#' \dontrun{
#'  library(rfasst);
#'  rfasst::beta_dongetal2021
#' }
#'
#'
"beta_dongetal2021"


#' cf_o3
#'
#' @description  Counterfactual threshold for ozone (Jerret et al 2009)
#' @source Jerrett, M., Burnett, R.T., Pope III, C.A., Ito, K., Thurston, G., Krewski, D., Shi, Y., Calle, E. and Thun, M., 2009. Long-term ozone exposure and mortality. New England Journal of Medicine, 360(11), pp.1085-1095.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::cf_o3
#' }
#'
#'
"cf_o3"

#' rr_resp_o3_Jerret2009_med
#'
#' @description Relative risk for respiratory disease associated to ozone exposure from Jerret et al 2009
#' @source TM5-FASST
#' \dontrun{
#'  library(rfasst);
#'  rfasst::rr_resp_o3_Jerret2009_med
#' }
#'
#'
"rr_resp_o3_Jerret2009_med"

#' rr_resp_o3_Jerret2009_low
#'
#' @description Relative risk for respiratory disease associated to ozone exposure from Jerret et al 2009
#' @source TM5-FASST
#' \dontrun{
#'  library(rfasst);
#'  rfasst::rr_resp_o3_Jerret2009_low
#' }
#'
#'
"rr_resp_o3_Jerret2009_low"

#' rr_resp_o3_Jerret2009_high
#'
#' @description Relative risk for respiratory disease associated to ozone exposure from Jerret et al 2009
#' @source TM5-FASST
#' \dontrun{
#'  library(rfasst);
#'  rfasst::rr_resp_o3_Jerret2009_high
#' }
#'
#'
"rr_resp_o3_Jerret2009_high"

#' rr_resp_o3_GBD2016_med
#'
#' @description Relative risk for respiratory disease associated to ozone exposure from GBD2016
#' @source TM5-FASST
#' \dontrun{
#'  library(rfasst);
#'  rfasst::rr_resp_o3_GBD2016_med
#' }
#'
#'
"rr_resp_o3_GBD2016_med"

#' rr_resp_o3_GBD2016_high
#'
#' @description Relative risk for respiratory disease associated to ozone exposure from GBD2016
#' @source TM5-FASST
#' \dontrun{
#'  library(rfasst);
#'  rfasst::rr_resp_o3_GBD2016_high
#' }
#'
#'
"rr_resp_o3_GBD2016_high"

#' rr_resp_o3_GBD2016_low
#'
#' @description Relative risk for respiratory disease associated to ozone exposure from GBD2016
#' @source TM5-FASST
#' \dontrun{
#'  library(rfasst);
#'  rfasst::rr_resp_o3_GBD2016_low
#' }
#'
#'
"rr_resp_o3_GBD2016_low"

#' dis
#'
#' @description List of diseases for reading relative risk
#' @source TM5-FASST
#' \dontrun{
#'  library(rfasst);
#'  rfasst::dis
#' }
#'
#'
"dis"

#' coef.AOT_MAIZE
#'
#' @description Coefficient for AOT40-Maize
#' @source Mills, G., Buse, A., Gimeno, B., Bermejo, V., Holland, M., Emberson, L. and Pleijel, H., 2007. A synthesis of AOT40-based response functions and critical levels of ozone for agricultural and horticultural crops. Atmospheric Environment, 41(12), pp.2630-2643.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::coef.AOT_MAIZE
#' }
#'
#'
"coef.AOT_MAIZE"

#' coef.AOT_RICE
#'
#' @description Coefficient for AOT40-Rice
#' @source Mills, G., Buse, A., Gimeno, B., Bermejo, V., Holland, M., Emberson, L. and Pleijel, H., 2007. A synthesis of AOT40-based response functions and critical levels of ozone for agricultural and horticultural crops. Atmospheric Environment, 41(12), pp.2630-2643.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::coef.AOT_RICE
#' }
#'
#'
"coef.AOT_RICE"

#' coef.AOT_SOY
#'
#' @description Coefficient for AOT40-Soy
#' @source Mills, G., Buse, A., Gimeno, B., Bermejo, V., Holland, M., Emberson, L. and Pleijel, H., 2007. A synthesis of AOT40-based response functions and critical levels of ozone for agricultural and horticultural crops. Atmospheric Environment, 41(12), pp.2630-2643.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::coef.AOT_SOY
#' }
#'
#'
"coef.AOT_SOY"

#' coef.AOT_WHEAT
#'
#' @description Coefficient for AOT40-Wheat
#' @source Mills, G., Buse, A., Gimeno, B., Bermejo, V., Holland, M., Emberson, L. and Pleijel, H., 2007. A synthesis of AOT40-based response functions and critical levels of ozone for agricultural and horticultural crops. Atmospheric Environment, 41(12), pp.2630-2643.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::coef.AOT_WHEAT
#' }
#'
#'
"coef.AOT_WHEAT"

#' coef.Mi_MAIZE
#'
#' @description Coefficient for Mi-Maize
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::coef.Mi_MAIZE
#' }
#'
#'
"coef.Mi_MAIZE"

#' coef.Mi_RICE
#'
#' @description Coefficient for Mi-Rice
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::coef.Mi_RICE
#' }
#'
#'
"coef.Mi_RICE"

#' coef.Mi_SOY
#'
#' @description Coefficient for Mi-Soy
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::coef.Mi_SOY
#' }
#'
#'
"coef.Mi_SOY"

#' coef.Mi_WHEAT
#'
#' @description Coefficient for Mi-Wheat
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::coef.Mi_WHEAT
#' }
#'
#'
"coef.Mi_WHEAT"

#' a.Mi_MAIZE
#'
#' @description "a" coefficient for Mi-Maize
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::a.Mi_MAIZE
#' }
#'
#'
"a.Mi_MAIZE"

#' a.Mi_RICE
#'
#' @description "a" coefficient for Mi-Rice
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::a.Mi_RICE
#' }
#'
#'
"a.Mi_RICE"

#' a.Mi_SOY
#'
#' @description "a" coefficient for Mi-Soy
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::a.Mi_SOY
#' }
#'
#'
"a.Mi_SOY"

#' a.Mi_WHEAT
#'
#' @description "a" coefficient for Mi-Wheat
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::a.Mi_WHEAT
#' }
#'
#'
"a.Mi_WHEAT"

#' b.Mi_MAIZE
#'
#' @description "b" coefficient for Mi-Maize
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::b.Mi_MAIZE
#' }
#'
#'
"b.Mi_MAIZE"

#' b.Mi_RICE
#'
#' @description "b" coefficient for Mi-Rice
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::b.Mi_RICE
#' }
#'
#'
"b.Mi_RICE"

#' b.Mi_SOY
#'
#' @description "b" coefficient for Mi-Soy
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::b.Mi_SOY
#' }
#'
#'
"b.Mi_SOY"

#' b.Mi_WHEAT
#'
#' @description "b" coefficient for Mi-Maize
#' @source Wang, X. and Mauzerall, D.L., 2004. Characterizing distributions of surface ozone and its impact on grain production in China, Japan and South Korea: 1990 and 2020. Atmospheric Environment, 38(26), pp.4383-4402.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::b.Mi_WHEAT
#' }
#'
#'
"b.Mi_WHEAT"

#' CONV_TG_T
#'
#' @description Unit converter: Teragram to tonne
#' \dontrun{
#'  library(rfasst);
#'  rfasst::CONV_TG_T
#' }
#'
#'
"CONV_TG_T"

#' CONV_1975_2010
#'
#' @description 1975-2010 deflator
#' \dontrun{
#'  library(rfasst);
#'  rfasst::CONV_1975_2010
#' }
#'
#'
"CONV_1975_2010"

#' CONV_OCawb_POM
#'
#' @description Transform OC from biogenic sources to POM
#' @source Kanakidou, M., Seinfeld, J.H., Pandis, S.N., Barnes, I., Dentener, F.J., Facchini, M.C., Dingenen, R.V., Ervens, B., Nenes, A.N.C.J.S.E., Nielsen, C.J. and Swietlicki, E., 2005. Organic aerosol and global climate modelling: a review. Atmospheric Chemistry and Physics, 5(4), pp.1053-1123.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::CONV_OCawb_POM
#' }
#'
#'
"CONV_OCawb_POM"

#' CONV_OC_POM
#'
#' @description Transform OC to POM
#' @source Kanakidou, M., Seinfeld, J.H., Pandis, S.N., Barnes, I., Dentener, F.J., Facchini, M.C., Dingenen, R.V., Ervens, B., Nenes, A.N.C.J.S.E., Nielsen, C.J. and Swietlicki, E., 2005. Organic aerosol and global climate modelling: a review. Atmospheric Chemistry and Physics, 5(4), pp.1053-1123.
#' \dontrun{
#'  library(rfasst);
#'  rfasst::CONV_OC_POM
#' }
#'
#'
"CONV_OC_POM"

#' MTC_MTCO2
#'
#' @description Transform MTC to MTCO2
#' \dontrun{
#'  library(rfasst);
#'  rfasst::MTC_MTCO2
#' }
#'
#'
"MTC_MTCO2"

#' TG_KG
#'
#' @description Transform Tg to Kg
#' \dontrun{
#'  library(rfasst);
#'  rfasst::TG_KG
#' }
#'
#'
"TG_KG"

#' CONV_MIL
#'
#' @description Transform $Million to $
#' \dontrun{
#'  library(rfasst);
#'  rfasst::CONV_MIL
#' }
#'
#'
"CONV_MIL"

#' CONV_BIL
#'
#' @description Transform $Billion to $
#' \dontrun{
#'  library(rfasst);
#'  rfasst::CONV_BIL
#' }
#'
#'
"CONV_BIL"











































