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


my_pol<- read.csv("inst/extdata/mapping/Pol_Adj.csv")
usethis::use_data(my_pol, overwrite = T)


# Countries to GCAM regions
d.iso <- read.csv("inst/extdata/mapping/iso_GCAM_regID_name.csv")
usethis::use_data(d.iso, overwrite = T)


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
  dplyr::mutate(age = gsub(" years", "", age))

usethis::use_data(raw.daly, overwrite = T)

# raw.yll.pm25
raw.yll.pm25 = read.csv("inst/extdata/IHME-GBD_2019_DATA-f0a8f511-1.csv") %>%
  dplyr::filter(rei == "Ambient particulate matter pollution")
usethis::use_data(raw.yll.pm25, overwrite = T)

raw.yll.o3 = read.csv("inst/extdata/IHME-GBD_2019_DATA-f0a8f511-1.csv") %>%
  dplyr::filter(rei == "Ambient ozone pollution")
usethis::use_data(raw.yll.o3, overwrite = T)
#------------------
# New mortality rates
#raw.mort.rates.old <- raw.mort.rates
#usethis::use_data(raw.mort.rates.old, overwrite = T)

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


#=========================================================
# Downscalling
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
























