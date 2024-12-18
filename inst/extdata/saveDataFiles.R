# Converting raw data into package data
library(usethis)
library(magrittr)
library(terra)
library(sf)

#=========================================================
# Constants
#=========================================================

selected_pollutants <- c("BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "NOx", "OC", "SO2", "SO2_1", "SO2_2", "SO2_3", "SO2_4",
                         "BC_AWB", "CH4_AWB", "CO_AWB", "N2O_AWB", "NH3_AWB", "NMVOC_AWB", "NOx_AWB", "OC_AWB", "SO2_1_AWB", "SO2_2_AWB", "SO2_3_AWB", "SO2_4_AWB",
                         "CH4_AGR",  "N2O_AGR", "NH3_AGR", "NOx_AGR")
usethis::use_data(selected_pollutants, overwrite = T)

# Years to be analyzed: c('2005','2010','2020','2030','2040','2050','2060','2070','2080','2090','2100')
all_years<-c('2005','2010','2020','2030','2040','2050','2060','2070','2080','2090','2100')
usethis::use_data(all_years, overwrite = T)

# Normalized CH4-O3 relation from Fiore et al (2008)
ch4_htap_pert<-77000000000
usethis::use_data(ch4_htap_pert, overwrite = T)

# Crops that are included in the analysis
CROP_ANALYSIS <- c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "FodderHerb_C4","Fruits", "Legumes", "MiscCrop", "NutsSeeds",
                   "OilCrop", "OilPalm", "OtherGrain", "OtherGrain_C4", "PalmFruit", "Rice", "RootTuber","Soybean", "SugarCrop",
                   "SugarCrop_C4", "Vegetables", "Wheat")
usethis::use_data(CROP_ANALYSIS, overwrite = T)


# Percentages to divide population between Russia and Russia Eastern
perc_pop_rus<-0.767
usethis::use_data(perc_pop_rus, overwrite = T)

perc_pop_rue<-0.233
usethis::use_data(perc_pop_rue, overwrite = T)


# Indicate the pollutants whose emissions are mapped (if map=T in m1_emissions_rescale)
map_pol<-c("BC","NH3","NMVOC","NOx","POM","SO2")
usethis::use_data(map_pol, overwrite = T)


# Set of values to monetize health damages
gdp_eu_2005<-32700
usethis::use_data(gdp_eu_2005, overwrite = T)

vsl_eu_2005_lb<-1.8*1E6
usethis::use_data(vsl_eu_2005_lb, overwrite = T)

vsl_eu_2005_ub<-5.4*1E6
usethis::use_data(vsl_eu_2005_ub, overwrite = T)

vsl_eu_2005<-(vsl_eu_2005_lb+vsl_eu_2005_ub)/2
usethis::use_data(vsl_eu_2005, overwrite = T)

inc_elas_vsl<-0.8
usethis::use_data(inc_elas_vsl, overwrite = T)

vsly_eu_2014<-158448
usethis::use_data(vsly_eu_2014, overwrite = T)

vsly_eu_2005<-vsly_eu_2014*gcamdata::gdp_deflator(2005,base_year = 2014)
usethis::use_data(vsly_eu_2005, overwrite = T)

beta_dongetal2021 <- -0.02108
usethis::use_data(beta_dongetal2021, overwrite = T)

# Counterfactual threshold for ozone (Jerret et al 2009)
cf_o3<-33.3
usethis::use_data(cf_o3, overwrite = T)

# Relative risk for respiratory disease associated to ozone exposure
rr_resp_o3_Jerret2009_med<- 0.0039221
usethis::use_data(rr_resp_o3_Jerret2009_med, overwrite = T)

rr_resp_o3_Jerret2009_low<- 0.00099503
usethis::use_data(rr_resp_o3_Jerret2009_low, overwrite = T)

rr_resp_o3_Jerret2009_high<- 0.0064851
usethis::use_data(rr_resp_o3_Jerret2009_high, overwrite = T)

rr_resp_o3_GBD2016_med<- 0.0083382
usethis::use_data(rr_resp_o3_GBD2016_med, overwrite = T)

rr_resp_o3_GBD2016_low<- 0.0030459
usethis::use_data(rr_resp_o3_GBD2016_low, overwrite = T)

rr_resp_o3_GBD2016_high<- 0.013926
usethis::use_data(rr_resp_o3_GBD2016_high, overwrite = T)

# List of diseases for readng relative risk
dis=c("alri","copd","ihd","stroke","lc")
usethis::use_data(dis, overwrite = T)

# List of coefficients for AOT40 based on Mills et al (2007)
coef.AOT_MAIZE<-0.00356
usethis::use_data(coef.AOT_MAIZE, overwrite = T)

coef.AOT_RICE<-0.00415
usethis::use_data(coef.AOT_RICE, overwrite = T)

coef.AOT_SOY<-0.0113
usethis::use_data(coef.AOT_SOY, overwrite = T)

coef.AOT_WHEAT<-0.0163
usethis::use_data(coef.AOT_WHEAT, overwrite = T)



# List of coefficients for Mi based on Wang and Mauzerall (2004)
coef.Mi_MAIZE<-20
usethis::use_data(coef.Mi_MAIZE, overwrite = T)

coef.Mi_RICE<-25
usethis::use_data(coef.Mi_RICE, overwrite = T)

coef.Mi_SOY<-20
usethis::use_data(coef.Mi_SOY, overwrite = T)

coef.Mi_WHEAT<-25
usethis::use_data(coef.Mi_WHEAT, overwrite = T)

a.Mi_MAIZE<-124
usethis::use_data(a.Mi_MAIZE, overwrite = T)

a.Mi_RICE<-202
usethis::use_data(a.Mi_RICE, overwrite = T)

a.Mi_SOY<-107
usethis::use_data(a.Mi_SOY, overwrite = T)

a.Mi_WHEAT<-137
usethis::use_data(a.Mi_WHEAT, overwrite = T)

b.Mi_MAIZE<-2.83
usethis::use_data(b.Mi_MAIZE, overwrite = T)

b.Mi_RICE<-2.47
usethis::use_data(b.Mi_RICE, overwrite = T)

b.Mi_SOY<-1.58
usethis::use_data(b.Mi_SOY, overwrite = T)

b.Mi_WHEAT<-2.34
usethis::use_data(b.Mi_WHEAT, overwrite = T)

# Unit conversion
CONV_TG_T <- 1e6
usethis::use_data(CONV_TG_T, overwrite = T)

CONV_1975_2010 <- 3.248
usethis::use_data(CONV_1975_2010, overwrite = T)

CONV_OCawb_POM<-1.8
usethis::use_data(CONV_OCawb_POM, overwrite = T)

CONV_OC_POM<-1.3
usethis::use_data(CONV_OC_POM, overwrite = T)

MTC_MTCO2<-3.67
usethis::use_data(MTC_MTCO2, overwrite = T)

TG_KG<-1E9
usethis::use_data(TG_KG, overwrite = T)

CONV_BIL<-1E9
usethis::use_data(CONV_BIL, overwrite = T)

CONV_MIL<-1E6
usethis::use_data(CONV_MIL, overwrite = T)


#=========================================================
# Mapping files
#=========================================================
# Regions in TM5-FASST - iso3
fasst_reg<-read.csv("inst/extdata/mapping/fasst_reg.csv") %>%
  dplyr::rename(subRegionAlt = iso3)
usethis::use_data(fasst_reg, overwrite = T)

# Regions in GCAM - iso3
GCAM_reg<-read.csv("inst/extdata/mapping/GCAM_Reg_Adj.csv") %>%
  dplyr::rename(`ISO 3` = ISO.3,
                `GCAM Region` = GCAM.Region)
usethis::use_data(GCAM_reg, overwrite = T)
GCAM_reg_EUR<-read.csv("inst/extdata/mapping/GCAM_Reg_Adj.csv") %>%
  dplyr::rename(`ISO 3` = ISO.3,
                `GCAM Region` = GCAM.Region)
usethis::use_data(GCAM_reg_EUR, overwrite = T)

# Countries - iso3
country_iso<-read.csv("inst/extdata/mapping/country_iso.csv") %>%
  dplyr::select(name, alpha.3)%>%
  dplyr::rename(country = name,
                iso3 = alpha.3)
usethis::use_data(country_iso, overwrite = T)

# Shares to distribute emissions of different species between Russia Eastern (RUE) and Western (RUS)
adj_rus<-read.csv("inst/extdata/mapping/Adj_Russia.csv") %>%
  tidyr::gather(Pollutant, perc, -COUNTRY) %>%
  dplyr::mutate(Pollutant = as.factor(Pollutant))
usethis::use_data(adj_rus, overwrite = T)

# Percentages to downscale GCAM emissions to country-level
Percen_ap<-tibble::as_tibble(dplyr::bind_rows(
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_BC_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "BC"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_CO_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "CO"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_NH3_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "NH3"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_NMVOC_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "NMVOC"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_NOx_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "NOx"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_OC_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "OC"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_SO2_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "SO2"))) %>%
  tidyr::gather(year, value, -Country_iso, -Sector, -ghg, -Fuel, -Unit) %>%
  dplyr::mutate(year = as.numeric(gsub("X", "", year))) %>%
  dplyr::group_by(Country_iso, ghg, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(iso = Country_iso) %>%
  dplyr::left_join(read.csv("inst/extdata/mapping/iso_GCAM_regID_name.csv") %>%
                     dplyr::select(iso, GCAM_region_name, country_name)
                   , by = "iso") %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::group_by(GCAM_region_name, ghg, year) %>%
  dplyr::mutate(value_reg = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Percentage = value / value_reg)


Percen_ghg<-tibble::as_tibble(dplyr::bind_rows(
  read.csv("inst/extdata/mapping/CH4_1970_2021.csv", skip = 9),
  read.csv("inst/extdata/mapping/CO2_1970_2021.csv", skip = 9),
  read.csv("inst/extdata/mapping/N2O_1970_2021.csv", skip = 9))) %>%
  tidyr::gather(year, value, -IPCC_annex, -C_group_IM24_sh, -Country_code_A3, -Name, -Substance) %>%
  dplyr::mutate(year = as.numeric(gsub("Y_", "", year))) %>%
  dplyr::select(iso = Country_code_A3, country_name = Name, ghg = Substance, year, value) %>%
  dplyr::group_by(iso, ghg, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(iso = tolower(iso)) %>%
  dplyr::left_join(read.csv("inst/extdata/mapping/iso_GCAM_regID_name.csv") %>%
                     dplyr::select(iso, GCAM_region_name, country_name)
                   , by = "iso") %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::group_by(GCAM_region_name, ghg, year) %>%
  dplyr::mutate(value_reg = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Percentage = value / value_reg)


Percen<-dplyr::bind_rows(Percen_ap,Percen_ghg) %>%
  dplyr::filter(year %in% c("2005", "2010", "2020")) %>%
  dplyr::select(-value, -value_reg) %>%
  tidyr::complete(tidyr::nesting(iso, country_name, GCAM_region_name, ghg), year = c(2005,2010,2020,2030,2040,2050,2060,2070,2080,2090,2100)) %>%
  dplyr::group_by(iso, country_name, ghg) %>%
  dplyr::mutate(Percentage = dplyr::if_else(is.na(Percentage), gcamdata::approx_fun(year, Percentage, rule = 2), Percentage)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country_name = toupper(country_name),
                iso = toupper(iso)) %>%
  dplyr::select(`GCAM Region` = GCAM_region_name, Country = country_name, `ISO 3` = iso, Pollutant = ghg, year, Percentage) %>%
  dplyr::mutate(Pollutant = dplyr::if_else(Pollutant == "OC", "POM", as.character(Pollutant)),
                Pollutant = as.factor(Pollutant),
                year = as.factor(year))
usethis::use_data(Percen, overwrite = T)

Percen_ap_EUR<-tibble::as_tibble(dplyr::bind_rows(
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_BC_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "BC"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_CO_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "CO"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_NH3_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "NH3"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_NMVOC_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "NMVOC"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_NOx_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "NOx"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_OC_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "OC"),
  read.csv("inst/extdata/mapping/CEDS_GBD-MAPS_SO2_global_emissions_by_country_sector_fuel_2020_v1.csv") %>% dplyr::mutate(ghg = "SO2"))) %>%
  tidyr::gather(year, value, -Country_iso, -Sector, -ghg, -Fuel, -Unit) %>%
  dplyr::mutate(year = as.numeric(gsub("X", "", year))) %>%
  dplyr::group_by(Country_iso, ghg, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(iso = Country_iso) %>%
  dplyr::left_join(read.csv("inst/extdata/mapping/iso_GCAM_regID_name_EUR.csv") %>%
                     dplyr::select(iso, GCAM_region_name, country_name)
                   , by = "iso") %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::group_by(GCAM_region_name, ghg, year) %>%
  dplyr::mutate(value_reg = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Percentage = value / value_reg)


Percen_ghg_EUR<-tibble::as_tibble(dplyr::bind_rows(
  read.csv("inst/extdata/mapping/CH4_1970_2021.csv", skip = 9),
  read.csv("inst/extdata/mapping/CO2_1970_2021.csv", skip = 9),
  read.csv("inst/extdata/mapping/N2O_1970_2021.csv", skip = 9))) %>%
  tidyr::gather(year, value, -IPCC_annex, -C_group_IM24_sh, -Country_code_A3, -Name, -Substance) %>%
  dplyr::mutate(year = as.numeric(gsub("Y_", "", year))) %>%
  dplyr::select(iso = Country_code_A3, country_name = Name, ghg = Substance, year, value) %>%
  dplyr::group_by(iso, ghg, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(iso = tolower(iso)) %>%
  dplyr::left_join(read.csv("inst/extdata/mapping/iso_GCAM_regID_name_EUR.csv") %>%
                     dplyr::select(iso, GCAM_region_name, country_name)
                   , by = "iso") %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::group_by(GCAM_region_name, ghg, year) %>%
  dplyr::mutate(value_reg = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Percentage = value / value_reg)


Percen_EUR<-dplyr::bind_rows(Percen_ap_EUR,Percen_ghg_EUR) %>%
  dplyr::filter(year %in% c("2005", "2010", "2020")) %>%
  dplyr::select(-value, -value_reg) %>%
  tidyr::complete(tidyr::nesting(iso, country_name, GCAM_region_name, ghg), year = c(2005,2010,2020,2030,2040,2050,2060,2070,2080,2090,2100)) %>%
  dplyr::group_by(iso, country_name, ghg) %>%
  dplyr::mutate(Percentage = dplyr::if_else(is.na(Percentage), gcamdata::approx_fun(year, Percentage, rule = 2), Percentage)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country_name = toupper(country_name),
                iso = toupper(iso)) %>%
  dplyr::select(`GCAM Region` = GCAM_region_name, Country = country_name, `ISO 3` = iso, Pollutant = ghg, year, Percentage) %>%
  dplyr::mutate(Pollutant = dplyr::if_else(Pollutant == "OC", "POM", as.character(Pollutant)),
                Pollutant = as.factor(Pollutant),
                year = as.factor(year))
usethis::use_data(Percen_EUR, overwrite = T)


my_pol<- read.csv("inst/extdata/mapping/Pol_Adj.csv")
usethis::use_data(my_pol, overwrite = T)


# Countries to GCAM regions
d.iso <- read.csv("inst/extdata/mapping/iso_GCAM_regID_name.csv")
usethis::use_data(d.iso, overwrite = T)

d.iso_EUR <- read.csv("inst/extdata/mapping/iso_GCAM_regID_name_EUR.csv")
usethis::use_data(d.iso_EUR, overwrite = T)


# O3 to GCAM commodities (based on their carbon fixation pathways; C3 and C4 categories)
d.gcam.commod.o3 <- read.csv("inst/extdata/mapping/GCAM_commod_map_o3.csv")
usethis::use_data(d.gcam.commod.o3, overwrite = T)

# Harvested area by crop for weights to map O3 crops to GCAM commodities
d.ha <- read.csv("inst/extdata/mapping/area_harvest.csv")
usethis::use_data(d.ha, overwrite = T)

# Combined regions:
Regions<-dplyr::left_join(fasst_reg %>% dplyr::rename(`ISO 3` = subRegionAlt, `FASST region` = fasst_region)
                          , GCAM_reg, by="ISO 3") %>%
  dplyr::mutate(ISO3 = as.factor(`ISO 3`)) %>%
  dplyr::select(-`ISO 3`) %>%
  dplyr::rename(COUNTRY = Country)
usethis::use_data(Regions, overwrite = T)

Regions_EUR<-dplyr::left_join(fasst_reg %>% dplyr::rename(`ISO 3` = subRegionAlt, `FASST region` = fasst_region)
                          , GCAM_reg_EUR, by="ISO 3") %>%
  dplyr::mutate(ISO3 = as.factor(`ISO 3`)) %>%
  dplyr::select(-`ISO 3`) %>%
  dplyr::rename(COUNTRY = Country)
usethis::use_data(Regions_EUR, overwrite = T)

Regions_ctry_NUTS3<-dplyr::left_join(fasst_reg %>% dplyr::rename(`ISO 3` = subRegionAlt, `FASST region` = fasst_region)
                          , GCAM_reg_EUR, by="ISO 3") %>%
  dplyr::mutate(ISO3 = as.factor(`ISO 3`)) %>%
  dplyr::select(-`ISO 3`) %>%
  dplyr::rename(COUNTRY = Country)
usethis::use_data(Regions_ctry_NUTS3, overwrite = T)


# Weights for O3 crops
d.weight.gcam <- dplyr::select(d.ha, crop, iso, harvested.area) %>% # Need harvested areas for each crop in each region
  dplyr::full_join(d.gcam.commod.o3, by = "crop") %>%
  dplyr::full_join(d.iso, by = "iso") %>%
  dplyr::filter(!is.na(GCAM_commod), !is.na(harvested.area)) %>%
  dplyr::group_by(GCAM_region_name, GCAM_commod, crop) %>%
  dplyr::select(GCAM_region_name, GCAM_commod, crop, harvested.area) %>%
  dplyr::summarise(harvested.area = sum(harvested.area, na.rm = T)) %>%
  dplyr::mutate(weight = harvested.area / (sum(harvested.area, na.rm = T))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-harvested.area) %>%
  dplyr::arrange(GCAM_region_name, GCAM_commod, crop)
usethis::use_data(d.weight.gcam, overwrite = T)

d.weight.gcam_EUR <- dplyr::select(d.ha, crop, iso, harvested.area) %>% # Need harvested areas for each crop in each region
  dplyr::full_join(d.gcam.commod.o3, by = "crop") %>%
  dplyr::full_join(d.iso, by = "iso") %>%
  dplyr::filter(!is.na(GCAM_commod), !is.na(harvested.area)) %>%
  dplyr::group_by(GCAM_region_name, GCAM_commod, crop) %>%
  dplyr::select(GCAM_region_name, GCAM_commod, crop, harvested.area) %>%
  dplyr::summarise(harvested.area = sum(harvested.area, na.rm = T)) %>%
  dplyr::mutate(weight = harvested.area / (sum(harvested.area, na.rm = T))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-harvested.area) %>%
  dplyr::arrange(GCAM_region_name, GCAM_commod, crop)
usethis::use_data(d.weight.gcam_EUR, overwrite = T)

# Shape subset for maps
fasstSubset <- rmap::mapCountries

fasstSubset<-fasstSubset %>%
  dplyr::mutate(subRegionAlt = as.character(subRegionAlt)) %>%
  dplyr::left_join(fasst_reg, by = "subRegionAlt") %>%
  dplyr::select(-subRegion) %>%
  dplyr::rename(subRegion = fasst_region) %>%
  dplyr::mutate(subRegionAlt = as.factor(subRegionAlt))
usethis::use_data(fasstSubset, overwrite = T)

#=========================================================
# Ancillary data
#=========================================================
rawDataFolder_ancillary = "inst/extdata/ancillary/"

# raw.ssp.data
raw.ssp.data = read.csv(paste0(rawDataFolder_ancillary,"SSP_database_v9_IIASA_WIC-POP.csv"),sep="\t")
usethis::use_data(raw.ssp.data, overwrite = T)

# raw.twn.pop
raw.twn.pop = read.csv(paste0(rawDataFolder_ancillary,"Taiwan_OECD_Env-Growth.csv"))
usethis::use_data(raw.twn.pop, overwrite = T)

# raw.gdp
raw.gdp = read.csv(paste0(rawDataFolder_ancillary,"iiasa_GDP_SSP.csv"))
usethis::use_data(raw.gdp, overwrite = T)

# raw NUTS 2021 pop
raw.nuts.pop <- readr::read_tsv("inst/extdata/estat_cens_21agr3.tsv.gz") %>%
  tidyr::separate(`freq,age,sex,unit,geo\\TIME_PERIOD`,
                  into = c("freq", "age", "sex", "unit", "geo"),
                  sep = ",") %>%
  dplyr::mutate(`2021` = as.numeric(gsub(" p", "", `2021`)),
                year = 2021,
                value = `2021`) %>%
  dplyr::select(-`2021`) %>%
  as.data.frame()
usethis::use_data(raw.nuts.pop, overwrite = T)

# weights by NUTS3 for each country
weight.nuts.pop <- raw.nuts.pop %>%
  dplyr::mutate(NUTS_LEVEL = nchar(geo)-2) %>%
  dplyr::mutate(NUTS0 = stringr::str_sub(geo, 1, 2)) %>%
  dplyr::filter(NUTS_LEVEL == 3) %>%
  dplyr::group_by(freq, sex, age, unit, year, NUTS_LEVEL, NUTS0) %>%
  dplyr::mutate(value_nuts0 = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(weight = value / value_nuts0)
usethis::use_data(weight.nuts.pop, overwrite = T)

# ssp population
pop.all.SSP1 = calc_pop(ssp = 'SSP1')
usethis::use_data(pop.all.SSP1, overwrite = T)
pop.all.SSP2 = calc_pop(ssp = 'SSP2')
usethis::use_data(pop.all.SSP2, overwrite = T)
pop.all.SSP3 = calc_pop(ssp = 'SSP3')
usethis::use_data(pop.all.SSP3, overwrite = T)
pop.all.SSP4 = calc_pop(ssp = 'SSP4')
usethis::use_data(pop.all.SSP4, overwrite = T)
pop.all.SSP5 = calc_pop(ssp = 'SSP5')
usethis::use_data(pop.all.SSP5, overwrite = T)

pop.all.str.SSP1 = calc_pop_rfasst_reg_str(ssp = 'SSP1')
usethis::use_data(pop.all.str.SSP1, overwrite = T)
pop.all.str.SSP2 = calc_pop_rfasst_reg_str(ssp = 'SSP2')
usethis::use_data(pop.all.str.SSP2, overwrite = T)
pop.all.str.SSP3 = calc_pop_rfasst_reg_str(ssp = 'SSP3')
usethis::use_data(pop.all.str.SSP3, overwrite = T)
pop.all.str.SSP4 = calc_pop_rfasst_reg_str(ssp = 'SSP4')
usethis::use_data(pop.all.str.SSP4, overwrite = T)
pop.all.str.SSP5 = calc_pop_rfasst_reg_str(ssp = 'SSP5')
usethis::use_data(pop.all.str.SSP5, overwrite = T)

pop.all.ctry_nuts3.str.SSP1 = calc_pop_ctry_nuts3_str(ssp = 'SSP1')
usethis::use_data(pop.all.ctry_nuts3.str.SSP1, overwrite = T)
pop.all.ctry_nuts3.str.SSP2 = calc_pop_ctry_nuts3_str(ssp = 'SSP2')
usethis::use_data(pop.all.ctry_nuts3.str.SSP2, overwrite = T)
pop.all.ctry_nuts3.str.SSP3 = calc_pop_ctry_nuts3_str(ssp = 'SSP3')
usethis::use_data(pop.all.ctry_nuts3.str.SSP3, overwrite = T)
pop.all.ctry_nuts3.str.SSP4 = calc_pop_ctry_nuts3_str(ssp = 'SSP4')
usethis::use_data(pop.all.ctry_nuts3.str.SSP4, overwrite = T)
pop.all.ctry_nuts3.str.SSP5 = calc_pop_ctry_nuts3_str(ssp = 'SSP5')
usethis::use_data(pop.all.ctry_nuts3.str.SSP5, overwrite = T)

# ssp gdp
gdp_pc.SSP1 = calc_gdp_pc(ssp = 'SSP1')
usethis::use_data(gdp_pc.SSP1, overwrite = T)
gdp_pc.SSP2 = calc_gdp_pc(ssp = 'SSP2')
usethis::use_data(gdp_pc.SSP2, overwrite = T)
gdp_pc.SSP3 = calc_gdp_pc(ssp = 'SSP3')
usethis::use_data(gdp_pc.SSP3, overwrite = T)
gdp_pc.SSP4 = calc_gdp_pc(ssp = 'SSP4')
usethis::use_data(gdp_pc.SSP4, overwrite = T)
gdp_pc.SSP5 = calc_gdp_pc(ssp = 'SSP5')
usethis::use_data(gdp_pc.SSP5, overwrite = T)

# Annual ssp-specific GDP growth rates (for HCL)
gdp_growth <- raw.gdp %>%
  tidyr::pivot_longer(cols = starts_with("X"),
                      names_to = "year",
                      values_to = "value") %>%
  dplyr::filter(complete.cases(.)) %>%
  # Add TM5-FASST regions
  gcamdata::left_join_error_no_match(fasst_reg %>% dplyr::rename(REGION = subRegionAlt), by = c('REGION')) %>%
  dplyr::group_by(scenario = SCENARIO, fasst_region, year, unit = UNIT) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(year = gsub("X", "", year),
                scenario = dplyr::if_else(grepl("SSP1", scenario), "SSP1", scenario),
                scenario = dplyr::if_else(grepl("SSP2", scenario), "SSP2", scenario),
                scenario = dplyr::if_else(grepl("SSP3", scenario), "SSP3", scenario),
                scenario = dplyr::if_else(grepl("SSP4", scenario), "SSP4", scenario),
                scenario = dplyr::if_else(grepl("SSP5", scenario), "SSP5", scenario),
                year = as.numeric(year)) %>%
  tidyr::complete(tidyr::nesting(scenario, fasst_region, unit), year = seq(2010, 2100, by = 1)) %>%
  dplyr::group_by(scenario, fasst_region, unit) %>%
  dplyr::mutate(value = gcamdata::approx_fun(year, value, rule = 1),
                value_lag = dplyr::lag(value)) %>%
  dplyr::mutate(growth = value / value_lag) %>%
  dplyr::filter(year %in% rfasst::all_years) %>%
  dplyr::select(scenario, region = fasst_region, year, growth, unit) %>%
  dplyr::mutate(growth = gcamdata::approx_fun(year, growth, rule = 2)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-unit)

  # ADD RUE
gdp_growth <- gdp_growth %>%
  dplyr::bind_rows(gdp_growth %>%
                     dplyr::filter(region == "RUS") %>%
                     dplyr::mutate(region = "RUE"))

gdp_growth.SSP1 <- gdp_growth %>% dplyr::filter(scenario == "SSP1")
usethis::use_data(gdp_growth.SSP1, overwrite = T)

gdp_growth.SSP2 <- gdp_growth %>% dplyr::filter(scenario == "SSP2")
usethis::use_data(gdp_growth.SSP2, overwrite = T)

gdp_growth.SSP3 <- gdp_growth %>% dplyr::filter(scenario == "SSP3")
usethis::use_data(gdp_growth.SSP3, overwrite = T)

gdp_growth.SSP4 <- gdp_growth %>% dplyr::filter(scenario == "SSP4")
usethis::use_data(gdp_growth.SSP4, overwrite = T)

gdp_growth.SSP5 <- gdp_growth %>% dplyr::filter(scenario == "SSP5")
usethis::use_data(gdp_growth.SSP5, overwrite = T)



#=========================================================
# Module 2
#=========================================================
rawDataFolder_m2 = "inst/extdata/module_2/"

# raw.base_conc
raw.base_conc = read.csv(paste0(rawDataFolder_m2,"Conc_base.csv"))
usethis::use_data(raw.base_conc, overwrite = T)

# raw.base_conc.nat
raw.base_conc.nat <- read.csv("inst/extdata/nat_pm.csv")
usethis::use_data(raw.base_conc.nat, overwrite = T)

# raw.base_em
raw.base_em = read.csv(paste0(rawDataFolder_m2,"EM_base.csv"))
usethis::use_data(raw.base_em, overwrite = T)

# raw.base_aot
raw.base_aot = read.csv(paste0(rawDataFolder_m2,"base_aot.csv"))
usethis::use_data(raw.base_aot, overwrite = T)

# raw.base_mi
raw.base_mi = read.csv(paste0(rawDataFolder_m2,"base_mi.csv"))
usethis::use_data(raw.base_mi, overwrite = T)


#------------------------------------------------------------
# COUNTRY-NUTS sf
#------------------------------------------------------------

# Load data
# World Map: Load using rnaturalearth
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::rename("id_code" = "adm0_a3")

# NUTS Regions: Load using sf
# Source: https://ec.europa.eu/eurostat/web/gisco/geodata/statistical-units/territorial-units-statistics
# NUTS 2424, SHP, Polygons (RG), 20M, EPSG: 4326
nuts <- sf::st_read("inst/extdata/NUTS_RG_20M_2024_4326.shp/NUTS_RG_20M_2024_4326.shp") %>%
  dplyr::rename("id_code" = "NUTS_ID")

# Filter NUTS regions: Europe
nuts_europe <- nuts[nuts$CNTR_CODE %in% unique(world$iso_a2[world$region_un == "Europe"]) &
                      nuts$LEVL_CODE == 3, ]

# Combine data
# Add a common column for differentiation
world$region_type <- "CTRY"
nuts_europe$region_type <- "NUTS"

# Ensure column names match between the two dataframes
world <- world[, c("region_type", "geometry", "id_code")]
nuts_europe <- nuts_europe[, c("region_type", "geometry", "id_code")]

# Combine the two sf objects
ctry_nuts_sf <- rbind(world, nuts_europe)

# Save it
usethis::use_data(ctry_nuts_sf, overwrite = T)

# Save also NUTS3 sf and NUTS based in the European continent sf
nuts_sf <- nuts_europe
usethis::use_data(nuts_sf, overwrite = T)
nuts_europe_sf <- nuts_europe %>%
  dplyr::filter(!id_code %in% c("FRG01", "FRG02", "FRG03", "FRG04", "FRG05", # FRA: French Guiana
                                "FRB01", "FRB02", "FRB03", "FRB04", "FRB05", "FRB06", # FRA: Guadelopue
                                "FRM01", "FRM02", # FRA: Martinique
                                "FRY10", "FRY20", "FRY30", "FRY40", "FRY50", # FRA: Reunion
                                "FRC11", "FRC12", "FRC13", "FRC14", "FRC21", "FRC22", "FRC23", "FRC24", # FRA: Saint Pierre and Miquelon
                                "PT200", "PT300", # POR: Azores & Madeira
                                "ES703", "ES704", "ES705", "ES706", "ES707", "ES708", "ES709", # ESP: Canary Islands
                                "NO0B1", "NO0B2")) # NOR: Svalab & Jan Mayen Islands
usethis::use_data(nuts_europe_sf, overwrite = T)


#------------------------------------------------------------
# PM2.5
#------------------------------------------------------------

# src.bc
src.bc = read.csv(paste0(rawDataFolder_m2,"bc_pop.csv"))
usethis::use_data(src.bc, overwrite = T)

# src.pom
src.pom = read.csv(paste0(rawDataFolder_m2,"pom_pop.csv"))
usethis::use_data(src.pom, overwrite = T)

# src.urb_incr
raw.urb_incr = read.csv(paste0(rawDataFolder_m2,"Urban_increment.csv"))
usethis::use_data(raw.urb_incr, overwrite = T)
#------------------------------------------------------------
#NO3

# src.no3_nox
src.no3_nox = read.csv(paste0(rawDataFolder_m2,"dno3_dnox.csv"),sep="\t")
usethis::use_data(src.no3_nox, overwrite = T)

# src.no3_so2
src.no3_so2 = read.csv(paste0(rawDataFolder_m2,"dno3_dso2.csv"),sep="\t")
usethis::use_data(src.no3_so2, overwrite = T)

# src.no3_nh3
src.no3_nh3 = read.csv(paste0(rawDataFolder_m2,"dno3_nh3.csv"),sep="\t")
usethis::use_data(src.no3_nh3, overwrite = T)
#------------------------------------------------------------
# SO4
# src.so4_nox
src.so4_nox = read.csv(paste0(rawDataFolder_m2,"dso4_dnox.csv"),sep="\t")
usethis::use_data(src.so4_nox, overwrite = T)

# src.so4_so2
src.so4_so2 = read.csv(paste0(rawDataFolder_m2,"dso4_dso2.csv"),sep="\t")
usethis::use_data(src.so4_so2, overwrite = T)

# src.so4_nh3
src.so4_nh3 = read.csv(paste0(rawDataFolder_m2,"dso4_dnh3.csv"),sep="\t")
usethis::use_data(src.so4_nh3, overwrite = T)
#------------------------------------------------------------
# NH4
# src.nh4_nox
src.nh4_nox = read.csv(paste0(rawDataFolder_m2,"dnh4_dnox.csv"),sep="\t")
usethis::use_data(src.nh4_nox, overwrite = T)

# src.nh4_so2
src.nh4_so2 = read.csv(paste0(rawDataFolder_m2,"dnh4_dso2.csv"),sep="\t")
usethis::use_data(src.nh4_so2, overwrite = T)

# src.nh4_nh3
src.nh4_nh3 = read.csv(paste0(rawDataFolder_m2,"dnh4_dnh3.csv"),sep="\t")
usethis::use_data(src.nh4_nh3, overwrite = T)
#------------------------------------------------------------
# O3
#------------------------------------------------------------
# src.o3_nox
src.o3_nox = read.csv(paste0(rawDataFolder_m2,"do3_dnox.csv"))
usethis::use_data(src.o3_nox, overwrite = T)

# src.o3_so2
src.o3_so2 = read.csv(paste0(rawDataFolder_m2,"do3_dso2.csv"),sep="\t")
usethis::use_data(src.o3_so2, overwrite = T)

# src.o3_nmvoc
src.o3_nmvoc = read.csv(paste0(rawDataFolder_m2,"do3_dnmvoc.csv"),sep="\t")
usethis::use_data(src.o3_nmvoc, overwrite = T)

# src.o3_ch4
src.o3_ch4 = read.csv(paste0(rawDataFolder_m2,"do3_dch4.csv"))
usethis::use_data(src.o3_ch4, overwrite = T)
#------------------------------------------------------------
# M6M
#------------------------------------------------------------
# src.m6m_nox
src.m6m_nox = read.csv(paste0(rawDataFolder_m2,"dm6m_dnox.csv"))
usethis::use_data(src.m6m_nox, overwrite = T)

# src.m6m_so2
src.m6m_so2 = read.csv(paste0(rawDataFolder_m2,"dm6m_dso2.csv"))
usethis::use_data(src.m6m_so2, overwrite = T)

# src.m6m_nmvoc
src.m6m_nmvoc = read.csv(paste0(rawDataFolder_m2,"dm6m_dnmvoc.csv"))
usethis::use_data(src.m6m_nmvoc, overwrite = T)

# src.m6m_ch4
src.m6m_ch4 = read.csv(paste0(rawDataFolder_m2,"dm6m_dch4.csv"))
usethis::use_data(src.m6m_ch4, overwrite = T)

#------------------------------------------------------------
# AOT40
#------------------------------------------------------------
# Maize

# src.maize_aot40_ch4
src.maize_aot40_ch4 = read.csv(paste0(rawDataFolder_m2,"daot40_maize_dch4.csv"),sep="\t")
usethis::use_data(src.maize_aot40_ch4, overwrite = T)

# src.maize_aot40_nmvoc
src.maize_aot40_nmvoc = read.csv(paste0(rawDataFolder_m2,"daot40_maize_dnmvoc.csv"),sep="\t")
usethis::use_data(src.maize_aot40_nmvoc, overwrite = T)

# src.maize_aot40_nox
src.maize_aot40_nox = read.csv(paste0(rawDataFolder_m2,"daot40_maize_dnox.csv"),sep="\t")
usethis::use_data(src.maize_aot40_nox, overwrite = T)

# src.maize_aot40_so2
src.maize_aot40_so2 = read.csv(paste0(rawDataFolder_m2,"daot40_maize_dso2.csv"),sep="\t")
usethis::use_data(src.maize_aot40_so2, overwrite = T)

#------------------------------------------------------------
# Rice

# src.rice_aot40_ch4
src.rice_aot40_ch4 = read.csv(paste0(rawDataFolder_m2,"daot40_rice_dch4.csv"),sep="\t")
usethis::use_data(src.rice_aot40_ch4, overwrite = T)

# src.rice_aot40_nmvoc
src.rice_aot40_nmvoc = read.csv(paste0(rawDataFolder_m2,"daot40_rice_dnmvoc.csv"),sep="\t")
usethis::use_data(src.rice_aot40_nmvoc, overwrite = T)

# src.rice_aot40_nox
src.rice_aot40_nox = read.csv(paste0(rawDataFolder_m2,"daot40_rice_dnox.csv"),sep="\t")
usethis::use_data(src.rice_aot40_nox, overwrite = T)

# src.rice_aot40_so2
src.rice_aot40_so2 = read.csv(paste0(rawDataFolder_m2,"daot40_rice_dso2.csv"),sep="\t")
usethis::use_data(src.rice_aot40_so2, overwrite = T)

#------------------------------------------------------------
# Soybeans

# src.soy_aot40_ch4
src.soy_aot40_ch4 = read.csv(paste0(rawDataFolder_m2,"daot40_soy_dch4.csv"),sep="\t")
usethis::use_data(src.soy_aot40_ch4, overwrite = T)

# src.soy_aot40_nmvoc
src.soy_aot40_nmvoc = read.csv(paste0(rawDataFolder_m2,"daot40_soy_dnmvoc.csv"),sep="\t")
usethis::use_data(src.soy_aot40_nmvoc, overwrite = T)

# src.soy_aot40_nox
src.soy_aot40_nox = read.csv(paste0(rawDataFolder_m2,"daot40_soy_dnox.csv"),sep="\t")
usethis::use_data(src.soy_aot40_nox, overwrite = T)

# src.soy_aot40_so2
src.soy_aot40_so2 = read.csv(paste0(rawDataFolder_m2,"daot40_soy_dso2.csv"),sep="\t")
usethis::use_data(src.soy_aot40_so2, overwrite = T)

#------------------------------------------------------------
# Wheat

# src.wheat_aot40_ch4
src.wheat_aot40_ch4 = read.csv(paste0(rawDataFolder_m2,"daot40_wheat_dch4.csv"),sep="\t")
usethis::use_data(src.wheat_aot40_ch4, overwrite = T)

# src.wheat_aot40_nmvoc
src.wheat_aot40_nmvoc = read.csv(paste0(rawDataFolder_m2,"daot40_wheat_dnmvoc.csv"),sep="\t")
usethis::use_data(src.wheat_aot40_nmvoc, overwrite = T)

# src.wheat_aot40_nox
src.wheat_aot40_nox = read.csv(paste0(rawDataFolder_m2,"daot40_wheat_dnox.csv"),sep="\t")
usethis::use_data(src.wheat_aot40_nox, overwrite = T)

# src.wheat_aot40_so2
src.wheat_aot40_so2 = read.csv(paste0(rawDataFolder_m2,"daot40_wheat_dso2.csv"),sep="\t")
usethis::use_data(src.wheat_aot40_so2, overwrite = T)

#------------------------------------------------------------
# Mi
#------------------------------------------------------------
# Maize

# src.maize_mi_ch4
src.maize_mi_ch4 = read.csv(paste0(rawDataFolder_m2,"dmi_maize_dch4.csv"),sep="\t")
usethis::use_data(src.maize_mi_ch4, overwrite = T)

# src.maize_mi_nmvoc
src.maize_mi_nmvoc = read.csv(paste0(rawDataFolder_m2,"dmi_maize_dnmvoc.csv"),sep="\t")
usethis::use_data(src.maize_mi_nmvoc, overwrite = T)

# src.maize_mi_nox
src.maize_mi_nox = read.csv(paste0(rawDataFolder_m2,"dmi_maize_dnox.csv"),sep="\t")
usethis::use_data(src.maize_mi_nox, overwrite = T)

# src.maize_mi_so2
src.maize_mi_so2 = read.csv(paste0(rawDataFolder_m2,"dmi_maize_dso2.csv"),sep="\t")
usethis::use_data(src.maize_mi_so2, overwrite = T)

#------------------------------------------------------------
# Rice

# src.rice_mi_ch4
src.rice_mi_ch4 = read.csv(paste0(rawDataFolder_m2,"dmi_rice_dch4.csv"),sep="\t")
usethis::use_data(src.rice_mi_ch4, overwrite = T)

# src.rice_mi_nmvoc
src.rice_mi_nmvoc = read.csv(paste0(rawDataFolder_m2,"dmi_rice_dnmvoc.csv"),sep="\t")
usethis::use_data(src.rice_mi_nmvoc, overwrite = T)

# src.rice_mi_nox
src.rice_mi_nox = read.csv(paste0(rawDataFolder_m2,"dmi_rice_dnox.csv"),sep="\t")
usethis::use_data(src.rice_mi_nox, overwrite = T)

# src.rice_mi_so2
src.rice_mi_so2 = read.csv(paste0(rawDataFolder_m2,"dmi_rice_dso2.csv"),sep="\t")
usethis::use_data(src.rice_mi_so2, overwrite = T)

#----------------------------------------------------------------------
# Soybeans

# src.soy_mi_ch4
src.soy_mi_ch4 = read.csv(paste0(rawDataFolder_m2,"dmi_soy_dch4.csv"))
usethis::use_data(src.soy_mi_ch4, overwrite = T)

# src.soy_mi_nmvoc
src.soy_mi_nmvoc = read.csv(paste0(rawDataFolder_m2,"dmi_soy_dnmvoc.csv"))
usethis::use_data(src.soy_mi_nmvoc, overwrite = T)

# src.soy_mi_nox
src.soy_mi_nox = read.csv(paste0(rawDataFolder_m2,"dmi_soy_dnox.csv"))
usethis::use_data(src.soy_mi_nox, overwrite = T)

# src.soy_mi_so2
src.soy_mi_so2 = read.csv(paste0(rawDataFolder_m2,"dmi_soy_dso2.csv"))
usethis::use_data(src.soy_mi_so2, overwrite = T)

#----------------------------------------------------------------------
# Wheat

# src.wheat_mi_ch4
src.wheat_mi_ch4 = read.csv(paste0(rawDataFolder_m2,"dmi_wheat_dch4.csv"),sep="\t")
usethis::use_data(src.wheat_mi_ch4, overwrite = T)

# src.wheat_mi_nmvoc
src.wheat_mi_nmvoc = read.csv(paste0(rawDataFolder_m2,"dmi_wheat_dnmvoc.csv"),sep="\t")
usethis::use_data(src.wheat_mi_nmvoc, overwrite = T)

# src.wheat_mi_nox
src.wheat_mi_nox = read.csv(paste0(rawDataFolder_m2,"dmi_wheat_dnox.csv"),sep="\t")
usethis::use_data(src.wheat_mi_nox, overwrite = T)

# src.wheat_mi_so2
src.wheat_mi_so2 = read.csv(paste0(rawDataFolder_m2,"dmi_wheat_dso2.csv"),sep="\t")
usethis::use_data(src.wheat_mi_so2, overwrite = T)

#=========================================================
# Module 3
#=========================================================
rawDataFolder_m3 = "inst/extdata/module_3/"

# Load RR-parameter files for the three methods

# 1 FUSION model
# raw.rr.fusion.allAges
raw.rr.fusion.allAges <- read.csv("inst/extdata/rr_fusion_allAges.csv") %>%
  tidyr::pivot_longer(cols = c("COPD", "LC", "LRI", "DM"),
                      names_to = "disease",
                      values_to = "rr") %>%
  dplyr::mutate(age = ">25")

# raw.rr.fusion.ihd
raw.rr.fusion.ihd <- read.csv("inst/extdata/rr_fusion_ihd.csv") %>%
  tidyr::pivot_longer(cols = starts_with("X"),
                      names_to = "age",
                      values_to = "rr") %>%
  dplyr::mutate(age = gsub("X", "", age),
                age = gsub("95.", "95+", age),
                age = gsub("and", "-", age))

# raw.rr.fusion.stroke
raw.rr.fusion.stroke <- read.csv("inst/extdata/rr_fusion_stroke.csv") %>%
  tidyr::pivot_longer(cols = starts_with("X"),
                      names_to = "age",
                      values_to = "rr") %>%
  dplyr::mutate(age = gsub("X", "", age),
                age = gsub("95.", "95+", age),
                age = gsub("and", "-", age))

# raw.rr.fusion
raw.rr.fusion <- dplyr::bind_rows(raw.rr.fusion.allAges,
                                  raw.rr.fusion.ihd,
                                  raw.rr.fusion.stroke) %>%
  dplyr::mutate(disease = tolower(disease))
usethis::use_data(raw.rr.fusion, overwrite = T)

# 2- GEMM
raw.rr.gemm.param <-read.csv("inst/extdata/GEMM parameteres.csv") %>%
  dplyr::mutate(age = gsub("and", "-", age)) %>%
  dplyr::mutate(disease = tolower(cause)) %>%
  dplyr::select(-cause) %>%
  dplyr::mutate(disease = dplyr::if_else(disease == "lung cancer", "lc", disease))
usethis::use_data(raw.rr.gemm.param, overwrite = T)

# 3- GBD (2018)
raw.rr.gbd.param <-read.csv("inst/extdata/GBD_param.csv") %>%
  dplyr::mutate(age = gsub("and", "-", age)) %>%
  dplyr::mutate(disease = tolower(cause)) %>%
  dplyr::select(-cause)
usethis::use_data(raw.rr.gbd.param, overwrite = T)


# raw.daly
raw.daly = dplyr::bind_rows(
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-1.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-2.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-3.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-4.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-5.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-6.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-7.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-8.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-9.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-10.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-11.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-12.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-13.csv"),
  read.csv("inst/extdata/IHME-GBD_2019_DATA-f0e0e3fd-14.csv")

) %>%
  dplyr::filter(year == max(as.numeric(year)),
                cause %in% c("Diabetes mellitus type 2", "Lower respiratory infections", "Ischemic stroke", "Ischemic heart disease", "Tracheal, bronchus, and lung cancer", "Chronic obstructive pulmonary disease"),
                age %in% (c("All ages", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years","55-59 years","60-64 years",
                       "65-69 years", "70-74 years", "75-79 years", "80-84", "85-89", "90-94", "95+ years"))) %>%
  dplyr::mutate(age = gsub(" years", "", age)) %>%
  dplyr::mutate(location = dplyr::if_else(location == "CÃ´te d'Ivoire", "Cote d'Ivoire", location))

usethis::use_data(raw.daly, overwrite = T)

# raw.yll.pm25
raw.yll.pm25 = read.csv("inst/extdata/IHME-GBD_2019_DATA-f0a8f511-1.csv") %>%
  dplyr::filter(rei == "Ambient particulate matter pollution") %>%
  dplyr::mutate(location = dplyr::if_else(location == "CÃ´te d'Ivoire", "Cote d'Ivoire", location))
usethis::use_data(raw.yll.pm25, overwrite = T)

raw.yll.o3 = read.csv("inst/extdata/IHME-GBD_2019_DATA-f0a8f511-1.csv") %>%
  dplyr::filter(rei == "Ambient ozone pollution") %>%
  dplyr::mutate(location = dplyr::if_else(location == "CÃ´te d'Ivoire", "Cote d'Ivoire", location))
usethis::use_data(raw.yll.o3, overwrite = T)
#------------------
# Mortality rates

# Simple mortality rate (by country, age, disease)
raw.mort.rates <- dplyr::bind_rows(
  read.csv("inst/extdata/mr_stroke.csv"),
  read.csv("inst/extdata/mr_ihd.csv"),
) %>%
  dplyr::mutate(age_name = gsub(" to ", "-", age_name),
                age_name = gsub(" plus", "+", age_name)) %>%
  dplyr::bind_rows(
    read.csv("inst/extdata/mr_lri.csv"),
    read.csv("inst/extdata/mr_lc.csv"),
    read.csv("inst/extdata/mr_copd.csv"),
    read.csv("inst/extdata/mr_dm.csv"),
  ) %>%
  dplyr::select(region = FASST.REGION, year = YEAR, age = age_name, disease = cause_name, rate = Selected) %>%
  dplyr::mutate(disease = tolower(disease),
                year = as.numeric(year)) %>%
  tidyr::complete(tidyr::nesting(region, age, disease), year = seq(1990, 2100, by = 5)) %>%
  dplyr::group_by(region, age, disease) %>%
  dplyr::mutate(rate = dplyr::if_else(is.na(rate), gcamdata::approx_fun(year, rate, rule = 2), rate))
usethis::use_data(raw.mort.rates, overwrite = T)



# Function to read all CSVs in a single ZIP
read_csvs_from_zip <- function(zip_file) {
  print(zip_file)
  # Get a temporary directory
  temp_dir <- tempdir()
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir)
  # Unzip files into the temporary directory
  unzip(zip_file, exdir = temp_dir)
  # Get all CSV file paths in the extracted folder
  csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
  # Read all CSV files into a list of data frames
  csv_data <- lapply(csv_files, read.csv)
  # Optionally combine into a single data frame (if they share structure)
  combined_data <- dplyr::bind_rows(csv_data, .id = "source_file")

  return(combined_data)
}


# Complete mortality rates (by country, age, disease, and sex)
ihme.population = read_csvs_from_zip('inst/extdata/IHME-GBD_2021_DATA-population/IHME-GBD_2021_DATA-768921ab-1.zip')
ihme.mort <- dplyr::bind_rows(lapply(list.files(path = 'inst/extdata/IHME-GBD_2021_DATA-mort/',
                                                    pattern = "\\.zip$", full.names = TRUE), read_csvs_from_zip))

iso_ihme_rfasst <- gcamdata::left_join_error_no_match(
  read.csv("inst/extdata/mapping/ihme_localtions.csv") %>% tibble::as_tibble(),
  read.csv("inst/extdata/mapping/iso_GCAM_regID_name_EUR.csv") %>% tibble::as_tibble(),
  by = "country_name") %>% gcamdata::left_join_error_no_match(
    Regions_EUR %>%
      dplyr::rename(iso = ISO3, GCAM_region_name = `GCAM Region`) %>%
      dplyr::mutate(iso = tolower(iso)),
    by = c('iso','GCAM_region_name'))

mort.rates.country <- ihme.mort %>% tibble::as_tibble() %>%
  gcamdata::left_join_error_no_match(iso_ihme_rfasst,
                                     by = "location_id")

raw.mort.rates.plus1 <- mort.rates.country %>%
  dplyr::filter(cause_name != 'Stroke') %>% # remove not accounted cause_name (ischemic stroke considered)
  dplyr::mutate(age_name = gsub(" years", "", age_name),
                age_name = gsub(" to ", "-", age_name),
                age_name = gsub(" plus", "+", age_name)) %>%
  dplyr::filter(metric_name == 'Rate', measure_name == 'Deaths') %>% # select only rate values from deaths
  dplyr::mutate(cause_name = tolower(cause_name),
                year = as.numeric(year)) %>%
  dplyr::mutate(cause_name = dplyr::if_else(cause_name == 'tracheal, bronchus, and lung cancer','lc',cause_name),
                cause_name = dplyr::if_else(cause_name == 'lower respiratory infections','lri',cause_name),
                cause_name = dplyr::if_else(cause_name == 'ischemic heart disease','ihd',cause_name),
                cause_name = dplyr::if_else(cause_name == 'ischemic stroke','stroke',cause_name),
                cause_name = dplyr::if_else(cause_name == 'chronic obstructive pulmonary disease','copd',cause_name),
                cause_name = dplyr::if_else(cause_name == 'diabetes mellitus type 2','dm',cause_name)
  ) %>%
  # ALRI only affects children < 5 years old, the rest of illnesses, >=25 years old
  dplyr::filter(dplyr::if_else(cause_name == 'lri',
                               age_name == '<5',
                               age_name %in% c("25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                                               "60-64","65-69","70-74","75-79","80-84","85-89","90-94",
                                               "95+","All Ages","All ages"))) %>%
  # select only years multiple of 5 and >= 1990
  dplyr::filter(year %% 5 == 0, year >= 1990) %>%
  # compute the regional rate
  gcamdata::left_join_error_no_match(ihme.population %>%
                                       dplyr::select(pop = val, location_id, sex_id, sex_name, age_id, year),
                                     by = c('location_id', 'sex_id', 'sex_name', 'age_id', 'year')) %>%
  dplyr::mutate(val = val * pop) %>%
  dplyr::group_by(region = `FASST region`, year, age = age_name, sex = sex_name, disease = cause_name) %>%
  dplyr::summarise(pop = sum(pop),
                   val = sum(val),
                   rate = val / pop) %>%
  dplyr::ungroup() %>%
  dplyr::select(-pop, -val)


# Expand to future years using the previous regression

# 1. 5-year step rate fluctuation by rfasst region
raw.mort.rates.fluctuation <- raw.mort.rates %>%
  dplyr::group_by(region, age, disease) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::mutate(fluc = (rate - dplyr::lag(rate)) / dplyr::lag(rate)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-rate) %>%
  dplyr::mutate(fluc = dplyr::if_else(is.na(fluc) | year <= 2020, 1, fluc)) %>%
  tidyr::complete(tidyr::nesting(region, disease, year), age = unique(raw.mort.rates.plus1$age), fill = list('fluc' = 1)) %>%
  dplyr::filter(dplyr::if_else(disease != 'lri', age != '<5', TRUE),
                dplyr::if_else(disease == 'lri', age == '<5', TRUE))

# 2. add this fluctuations into raw.mort.rates.plus
raw.mort.rates.plus2 <- raw.mort.rates.plus1 %>%
  tidyr::complete(tidyr::nesting(region, age, sex, disease), year = seq(1990, 2100, by = 5), fill = list('rate')) %>%
  dplyr::group_by(region, age, sex, disease) %>%
  dplyr::mutate(rate = dplyr::if_else(year > 2020, # if year > 2020, set 2020 rate
                                      max(rate[year == 2020], na.rm = TRUE), rate)) %>%
  dplyr::ungroup() %>%
  gcamdata::left_join_error_no_match(raw.mort.rates.fluctuation,
                                     by = c('region','age','disease','year')) %>%
  dplyr::mutate(rate = dplyr::if_else(year > 2020, rate + rate * fluc, rate)) %>%
  dplyr::group_by(region, age, sex, disease) %>%
  dplyr::mutate(rate = dplyr::if_else(year > 2040, # if year > 2040, set 2040 rate (last year with data)
                                      max(rate[year == 2040], na.rm = TRUE), rate)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-fluc)

raw.mort.rates.plus.rue <- raw.mort.rates.plus2 %>%
  dplyr::filter(region == 'RUS') %>%
  dplyr::mutate(region = 'RUE')

raw.mort.rates.plus <- dplyr::bind_rows(
  raw.mort.rates.plus2,
  raw.mort.rates.plus.rue
)

usethis::use_data(raw.mort.rates.plus, overwrite = T)



# CTRY-NUTS3 mortality rates (by country, age, disease, and sex)
raw.mort.rates.ctry_nuts1 <- mort.rates.country %>%
  dplyr::filter(cause_name != 'Stroke') %>% # remove not accounted cause_name (ischemic stroke considered)
  dplyr::mutate(age_name = gsub(" years", "", age_name),
                age_name = gsub(" to ", "-", age_name),
                age_name = gsub(" plus", "+", age_name)) %>%
  dplyr::filter(metric_name == 'Rate', measure_name == 'Deaths') %>% # select only rate values from deaths
  dplyr::mutate(cause_name = tolower(cause_name),
                year = as.numeric(year)) %>%
  dplyr::mutate(cause_name = dplyr::if_else(cause_name == 'tracheal, bronchus, and lung cancer','lc',cause_name),
                cause_name = dplyr::if_else(cause_name == 'lower respiratory infections','lri',cause_name),
                cause_name = dplyr::if_else(cause_name == 'ischemic heart disease','ihd',cause_name),
                cause_name = dplyr::if_else(cause_name == 'ischemic stroke','stroke',cause_name),
                cause_name = dplyr::if_else(cause_name == 'chronic obstructive pulmonary disease','copd',cause_name),
                cause_name = dplyr::if_else(cause_name == 'diabetes mellitus type 2','dm',cause_name)
  ) %>%
  # ALRI only affects children < 5 years old, the rest of illnesses, >=25 years old
  dplyr::filter(dplyr::if_else(cause_name == 'lri',
                               age_name == '<5',
                               age_name %in% c("25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                                               "60-64","65-69","70-74","75-79","80-84","85-89","90-94",
                                               "95+","All Ages","All ages"))) %>%
  # select only years multiple of 5 and >= 1990
  dplyr::filter(year %% 5 == 0, year >= 1990) %>%
  # compute the regional rate
  gcamdata::left_join_error_no_match(ihme.population %>%
                                       dplyr::select(pop = val, location_id, sex_id, sex_name, age_id, year),
                                     by = c('location_id', 'sex_id', 'sex_name', 'age_id', 'year')) %>%
  dplyr::mutate(val = val * pop) %>%
  dplyr::group_by(region = iso, year, age = age_name, sex = sex_name, disease = cause_name) %>%
  dplyr::summarise(pop = sum(pop),
                   val = sum(val),
                   rate = val / pop) %>%
  dplyr::ungroup() %>%
  dplyr::select(-pop, -val)


# Expand to future years using the previous regression

# 1. 5-year step rate fluctuation by rfasst region
raw.mort.rates.fluctuation <- raw.mort.rates %>%
  dplyr::group_by(region, age, disease) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::mutate(fluc = (rate - dplyr::lag(rate)) / dplyr::lag(rate)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-rate) %>%
  dplyr::mutate(fluc = dplyr::if_else(is.na(fluc) | year <= 2020, 1, fluc)) %>%
  tidyr::complete(tidyr::nesting(region, disease, year), age = unique(raw.mort.rates.ctry_nuts1$age), fill = list('fluc' = 1)) %>%
  dplyr::filter(dplyr::if_else(disease != 'lri', age != '<5', TRUE),
                dplyr::if_else(disease == 'lri', age == '<5', TRUE)) %>%
  dplyr::left_join(iso_ihme_rfasst %>%
                     dplyr::select(iso, region = `FASST region`) %>%
                     dplyr::mutate(iso = dplyr::if_else(region == 'RUE', 'rus', iso)),
                   by = 'region', relationship = "many-to-many") %>%
  dplyr::mutate(iso = toupper(iso)) %>%
  dplyr::select(region = iso, disease, year, age, fluc) %>%
  dplyr::distinct()

# 2. add this fluctuations into raw.mort.rates.plus
raw.mort.rates.ctry_nuts2 <- raw.mort.rates.ctry_nuts1 %>%
  tidyr::complete(tidyr::nesting(region, age, sex, disease), year = seq(1990, 2100, by = 5), fill = list('rate')) %>%
  dplyr::group_by(region, age, sex, disease) %>%
  dplyr::mutate(rate = dplyr::if_else(year > 2020, # if year > 2020, set 2020 rate
                                      max(rate[year == 2020], na.rm = TRUE), rate),
                region = toupper(region)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  gcamdata::left_join_error_no_match(raw.mort.rates.fluctuation,
                                     by = c('region','age','disease','year')) %>%
  dplyr::mutate(rate = dplyr::if_else(year > 2020, rate + rate * fluc, rate)) %>%
  dplyr::group_by(region, age, sex, disease) %>%
  dplyr::mutate(rate = dplyr::if_else(year > 2040, # if year > 2040, set 2040 rate (last year with data)
                                      max(rate[year == 2040], na.rm = TRUE), rate)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-fluc)

raw.mort.rates.ctry_nuts3 <- raw.mort.rates.ctry_nuts2 %>%
  dplyr::rename('ISO3' = 'region') %>%
  dplyr::left_join(ctry_nuts3_codes,
                   by = "ISO3", relationship = "many-to-many") %>%
  dplyr::select(-ISO3, -ISO2) %>%
  dplyr::filter(!is.na(NUTS3)) %>%
  dplyr::select(region = NUTS3, age, sex, disease, year, rate)

# Add missing countries
raw.mort.rates.ctry_nuts4 <-
  tidyr::expand_grid(GCAM_reg_EUR_NUTS3 %>%
                       dplyr::select(`Fasst Region` = region, region = NUTS3),
                     raw.mort.rates.ctry_nuts3 %>%
                       dplyr::select(age, sex, disease, year) %>%
                       dplyr::distinct()) %>%
  dplyr::mutate(region = dplyr::if_else(grepl('EL',region) & `Fasst Region` == 'GRC',
                                        gsub('EL','GR',region), region))

raw.mort.rates.ctry_nuts5 <- raw.mort.rates.ctry_nuts4 %>%
  dplyr::left_join(raw.mort.rates.ctry_nuts3,
                   by = c('region','age','sex','disease','year')) %>%
  dplyr::group_by(`Fasst Region`, age, sex, disease, year) %>%
  dplyr::mutate(mean_rate = mean(rate, na.rm = TRUE)) %>%
  dplyr::mutate(rate = dplyr::if_else(is.na(rate), mean_rate, rate)) %>%
  dplyr::ungroup() %>%
  dplyr::select(region, age, sex, disease, year, rate)

raw.mort.rates.ctry_nuts3 <- raw.mort.rates.ctry_nuts5
usethis::use_data(raw.mort.rates.ctry_nuts3, overwrite = T)


# ctry_nuts3_codes contains the NUTS3 codes for the European regions, and the ISO3 codes for the rest of the world.
ctry_nuts3_codes <- jsonlite::fromJSON('inst/extdata/iso2-iso3.json') %>%
  dplyr::select(ISO2 = iso2_code, ISO3 = iso3_code) %>%
  dplyr::left_join(
    sf::read_sf("inst/extdata/NUTS_RG_20M_2021_4326.shp") %>%
      tibble::as_tibble() %>%
      dplyr::filter(LEVL_CODE == 3) %>%
      dplyr::filter(!NUTS_ID %in% c("FRY2", "FRY20", "FRY1", "FRY10", "FRY3", "FRY30", "FRY4", "FRY40", "FR5", "FRY50",
                                    "PT2", "PT20", "PT3", "PT30", "PT300")) %>%
      dplyr::select(NUTS3 = NUTS_ID, ISO2 = CNTR_CODE) %>%
      # fix Greece (EL = GR) and United Kingdom (UK = GB)
      dplyr::mutate(NUTS3 = dplyr::if_else(ISO2 == 'EL',gsub('EL','GR',NUTS3), NUTS3),
                    ISO2 = dplyr::if_else(ISO2 == 'EL',gsub('EL','GR',ISO2), ISO2),
                    NUTS3 = dplyr::if_else(ISO2 == 'UK',gsub('UK','GB',NUTS3), NUTS3),
                    ISO2 = dplyr::if_else(ISO2 == 'UK',gsub('UK','GB',ISO2), ISO2)),
    by = 'ISO2') %>%
  dplyr::mutate(NUTS3 = dplyr::if_else(is.na(NUTS3), ISO3, NUTS3)) %>%
  dplyr::distinct()
# add Greece, United Kingdom, and Romania with the 2 possible ISO codes
ctry_nuts3_codes <- dplyr::bind_rows(
  ctry_nuts3_codes,
  ctry_nuts3_codes %>%
    dplyr::mutate(
      # Greece
      NUTS3 = dplyr::if_else(ISO3 == 'GRC', gsub("GR", "EL", NUTS3), NUTS3),
      # United Kingdom
      NUTS3 = dplyr::if_else(ISO3 == 'GBR', gsub("GB", "UK", NUTS3), NUTS3),
      # United Kingdom
      ISO3 = dplyr::if_else(ISO3 == 'ROU', "ROM", ISO3)
    )
  ) %>%
  dplyr::distinct()
usethis::use_data(ctry_nuts3_codes, overwrite = T)

# TODO start --------------------------------------------------
# raw.mort.rates.nuts3
# mapping for nuts3 and iso3 codes
nuts3_data <- sf::read_sf("inst/extdata/NUTS_RG_20M_2021_4326.shp") %>%
  tibble::as_tibble() %>%
  dplyr::filter(LEVL_CODE == 3) %>%
  dplyr::filter(!NUTS_ID %in% c("FRY2", "FRY20", "FRY1", "FRY10", "FRY3", "FRY30", "FRY4", "FRY40", "FR5", "FRY50",
                                "PT2", "PT20", "PT3", "PT30", "PT300")) %>%
  dplyr::select(NUTS3 = NUTS_ID, ISO2 = CNTR_CODE) %>%
  dplyr::left_join(jsonlite::fromJSON('inst/extdata/iso2-iso3.json') %>%
                     dplyr::select(ISO2 = iso2_code, ISO3 = iso3_code), by = 'ISO2') %>%
  # fix Greece (EL = GR) and United Kingdom (UK = GB)
  dplyr::mutate(ISO3 = dplyr::if_else(ISO2 == 'EL','GRC',ISO3)) %>%
  dplyr::mutate(ISO3 = dplyr::if_else(ISO2 == 'UK','GBR',ISO3)) %>%
  dplyr::distinct()

GCAM_reg_EUR_NUTS3 <- Regions_EUR %>% tibble::as_tibble() %>%
  dplyr::left_join(nuts3_data, by = 'ISO3') %>%
  dplyr::mutate(NUTS3 = dplyr::if_else(is.na(NUTS3), ISO3, NUTS3)) %>%
  dplyr::select(-ISO2, region = `FASST region`, `GCAM Region`, ISO3, NUTS3)

usethis::use_data(GCAM_reg_EUR_NUTS3, overwrite = T)

# expand raw.mort.rates to NUTS3
raw.mort.rates.nuts <- raw.mort.rates %>%
  dplyr::left_join(GCAM_reg_EUR_NUTS3, by = 'region', relationship = "many-to-many")


# raw.mort.rates.grid

# TODO end ------------------------------------------------

#=========================================================
# Downscaling
#=========================================================
countries <- rworldmap::countryExData %>%
  dplyr::select(subRegionAlt = ISO3V10) %>%
  dplyr::bind_rows(
    as.data.frame(rmap::mapCountries) %>%
      select(subRegionAlt)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(subRegionAlt=as.character(subRegionAlt)) %>%
  dplyr::left_join(rfasst::fasst_reg,by="subRegionAlt") %>%
  dplyr::rename(subRegion=fasst_region) %>%
  dplyr::mutate(subRegionAlt=as.factor(subRegionAlt)) %>%
  dplyr::select(region = subRegion, iso = subRegionAlt) %>%
  dplyr::filter(region != "RUE") %>%
  # add manually missing iso codes
  dplyr::bind_rows(
    data.frame(
      region = c("EAF"),
      iso = c("SSD")
    )
  ) %>%
  dplyr::arrange(region)

names(countries) <- c('n', 'ISO3V10')
usethis::use_data(countries, overwrite = T)

nuts3 <- vect("NUTS_RG_20M_2021_4326.shp")
terra::saveRDS(wrap(nuts3), "nuts3.rds")
# To read:
# nuts3 <- vect(readRDS("nuts3.rds"))
# change to sf as needed


rast_country <- terra::rast("rast_country.tif")
terra::saveRDS(wrap(rast_country), "rast_country.rds")
# To read:
# rast_country <- rast(readRDS("rast_country.rds"))


pm25_weights <- terra::rast("pm25_weights_rast.tif")
terra::saveRDS(wrap(pm25_weights), "pm25_weights.rds")
# To read:
# pm25_weights <- rast(readRDS("pm25_weights.rds"))
























