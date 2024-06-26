% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/m3_health.R
\name{m3_get_daly_o3}
\alias{m3_get_daly_o3}
\title{m3_get_daly_o3}
\source{
Institute for Health Metrics and Evaluation (http://www.healthdata.org/)
}
\usage{
m3_get_daly_o3(
  db_path = NULL,
  query_path = "./inst/extdata",
  db_name = NULL,
  prj_name = NULL,
  prj = NULL,
  rdata_name = NULL,
  scen_name,
  queries = "queries_rfasst.xml",
  final_db_year = 2100,
  mort_param = "Jerret2009",
  ssp = "SSP2",
  saveOutput = T,
  map = F,
  anim = T,
  recompute = F
)
}
\arguments{
\item{db_path}{Path to the GCAM database}

\item{query_path}{Path to the query file}

\item{db_name}{Name of the GCAM database}

\item{prj_name}{Name of the rgcam project. This can be an existing project, or, if not, this will be the name}

\item{prj}{rgcam loaded project}

\item{rdata_name}{Name of the RData file. It must contain the queries in a list}

\item{scen_name}{Vector names of the GCAM scenarios to be processed}

\item{queries}{Name of the GCAM query file. The file by default includes the queries required to run rfasst}

\item{final_db_year}{Final year in the GCAM database (this allows to process databases with user-defined "stop periods")}

\item{mort_param}{Method to compute the premature mortality estimates used in the plotting: Jerret2009, GBD2016}

\item{ssp}{Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2}

\item{saveOutput}{Writes the emission files.By default=T}

\item{map}{Produce the maps. By default=F}

\item{anim}{If set to T, produces multi-year animations. By default=T}

\item{recompute}{If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F}
}
\value{
Disability Adjusted Life Years (DALYs) attributable to O3 exposure for each TM5-FASST regions for all years (# DALYs). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
}
\description{
Produce Disability Adjusted Life Years (DALYs) attributable to O3 (M6M) exposure. See calc_daly_o3 for detials on DALY-to-Mortality ratios.
}
\keyword{DALY,}
\keyword{O3,}
\keyword{module_3,}
