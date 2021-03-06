% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_timeseries}
\alias{plot_timeseries}
\title{Plots time series}
\usage{
plot_timeseries(util, energy)
}
\arguments{
\item{util}{A data frame with columns: OAT, usage, estimated (1 for estimated usage and 0 for actual usage). See \code{\link{unretrofit_utility}} and \code{\link{retrofit_utility}}for more information about data format.}

\item{energy}{A character string. Energy Type, either 'Elec' or 'Fuel'.}
}
\description{
This function plots time series graph for date against usage and OAT. This function can be used for both retrofit and unretrofit data.
}
\examples{
\dontrun{util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
			& unretrofit_utility$energy_type == 'Elec')
plot_timeseries(util, 'Elec')}
}
\seealso{
\code{\link{plot_point}} and \code{\link{main_line_point_plot}}
}
