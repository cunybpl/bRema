#' Energy usage data (unretrofitted) of 23 buildings.
#'
#' A dataset containing nomarlized (by squarefoot) energy usage (unretrofitted) data of 23 builidings. Un
#'
#' @format A data frame with 1023 rows and 8 variables:
#' \describe{
#'   \item{bdbid}{building id (anonymized)}
#'   \item{usage}{normalized energy usage per day per square foot (kWh for Electricity and BTU for fuel).}
#'   \item{OAT}{monthly average outside air temperature in Fahrenheit}
#'   \item{fiscal_year}{fiscal year}
#'   \item{energy_type}{Energy type: Elec (Electricity) and Fuel}
#'   \item{prepost}{this indicates whether energy usage data was collected during retrofit period or not. If the data is unretrofit, this column will be all ones. If the data is retrofit, this column will be: 1 for pre-retrotit, 2 for retrofit and 3 for post-retrofit. See \code{\link{retrofit_utility}} for more information about retrofitted data.}
#'   \item{estimated}{this indicates whether energy usage data is estimated or actual data. 1 for estimated and 0 for acutal.}
#'   \item{end_date}{end date of the month. Format: yyyy-mm-d}
#' }
"unretrofit_utility"

#' Energy usage data (retrofitted) of 4 buildings
#'
#' A dataset containing energy usage (retrofitted) data of 23 builidings.
#' @format A data frame with 194 rows and 8 variables:
#' \describe{
#'   \item{bdbid}{building id (anonymized)}
#'   \item{usage}{energy usage per day (kWh for Electricity and BTU for fuel).}
#'   \item{OAT}{monthly average outside air temperature in Fahrenheit}
#'   \item{fiscal_year}{fiscal year}
#'   \item{energy_type}{Energy type: Elec (Electricity) and Fuel}
#'   \item{prepost}{this indicates whether energy usage data was collected during retrofit period or not. If the data is unretrofit, this column will be all ones. If the data is retrofit, this column will be: 1 for pre-retrotit, 2 for retrofit and 3 for post-retrofit. See \code{\link{unretrofit_utility}} for more information about unretrofitted data.}
#'   \item{estimated}{this indicates whether energy usage data is estimated or actual data. 1 for estimated and 0 for acutal.}
#'   \item{end_date}{end date of the month. Format: yyyy-mm-d}
#' }
"retrofit_utility"


#' Normalized average monthly temperature
#'
#' A dataset containing normalized average monthly temperature (unit F) TMY3 from Laguardia airport.
#' @format A data frame with 12 rows and 2 variables:
#' \describe{
#'   \item{month}{month}
#'   \item{avg_temp}{normalized average monthly temperature in Fahrenheit} 
#' }
"norm_temp_tmy3"