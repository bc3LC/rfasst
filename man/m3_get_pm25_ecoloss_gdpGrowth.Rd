% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/m3b_health_ecoloss.R
\name{m3_get_pm25_ecoloss_gdpGrowth}
\alias{m3_get_pm25_ecoloss_gdpGrowth}
\title{m3_get_pm25_ecoloss_gdpGrowth}
\source{
Dong, D., Xu, B., Shen, N. and He, Q., 2021. The adverse impact of air pollution on China’s economic growth. Sustainability, 13(16), p.9056.
}
\usage{
m3_get_pm25_ecoloss_gdpGrowth(
  db_path = NULL,
  query_path = "./inst/extdata",
  db_name = NULL,
  prj_name,
  prj = NULL,
  rdata_name = NULL,
  scen_name,
  ssp = "SSP2",
  final_db_year = 2100,
  mort_model = "GBD",
  inc_elas = 0.8,
  queries = "queries_rfasst.xml",
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

\item{ssp}{Set the ssp narrative associated to the GCAM scenario. c("SSP1","SSP2","SSP3","SSP4","SSP5"). By default is SSP2}

\item{final_db_year}{Final year in the GCAM database (this allows to process databases with user-defined "stop periods")}

\item{mort_model}{Select the health impact model (GBD, GEMM, or FUSION). By default = GBD}

\item{inc_elas}{Select the income elasticity. Normally c(0.8, 1, 1.2). By default = 0.8}

\item{queries}{Name of the GCAM query file. The file by default includes the queries required to run rfasst}

\item{saveOutput}{Writes the emission files.By default=T}

\item{map}{Produce the maps. By default=F}

\item{anim}{If set to T, produces multi-year animations. By default=T}

\item{recompute}{If set to T, recomputes the function output. Otherwise, if the output was already computed once, it uses that value and avoids repeating computations. By default=F}
}
\value{
Impact on GDP growth associated with PM2.5 exposure for each TM5-FASST regions for all years (%). The list of countries that form each region and the full name of the region can be found in Table S2.2 in the TM5-FASST documentation paper: Van Dingenen, R., Dentener, F., Crippa, M., Leitao, J., Marmer, E., Rao, S., Solazzo, E. and Valentini, L., 2018. TM5-FASST: a global atmospheric source-receptor model for rapid impact analysis of emission changes on air quality and short-lived climate pollutants. Atmospheric Chemistry and Physics, 18(21), pp.16173-16211.
}
\description{
Produce reduced GDP growth related to PM2.5 concentration.
}
\keyword{GDP}
\keyword{PM2.5}
\keyword{growth,}
\keyword{module_3,}
