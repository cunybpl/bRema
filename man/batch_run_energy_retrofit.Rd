% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrofit_controller.R
\name{batch_run_energy_retrofit}
\alias{batch_run_energy_retrofit}
\title{Batch run for specific energy (retrofit)}
\usage{
batch_run_energy_retrofit(utility, energy, metric_vec = TRUE,
  plot_flag = FALSE, step = 0.5, CV_RMSE_n = 0.25,
  Rsquared_n = 0.75, n_pre = 4, n_post = 4)
}
\arguments{
\item{utility}{A utility data frame of retrofitted data with multiple buildings.}

\item{energy}{A character string. Energy Type, either 'Elec' or 'Fuel'.}

\item{metric_vec}{A numeric (integer) vector. This vector determines the pirority of model types. Defaults to \code{c(1,2,3,4,5)} for models: '2P', '3PC', '3PH', '4P', '5P'.}

\item{plot_flag}{A boolean value. Defaults to \code{FALSE}. If set to \code{FALSE}, it will not return any plots.}

\item{step}{A single numeric value or a vector. If it is a single numeric value, \code{create_model} will be used to find change-point(s). If it is a vector with more than one component, \code{create_model_2} will be used. Defaults to 0.5. Adjust \code{step}-size depending on independent varaiables.}

\item{CV_RMSE_n}{A numeric value. CV-RMSE threshold value. Defaults to 0.25.}

\item{Rsquared_n}{A numeric value. Rsquared threshold value. Defaults to 0.75.}

\item{n_pre}{A numeric value that determines threshold for population test for pre-retrofit: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.}

\item{n_post}{A numeric value that determines threshold for population test for post-retrofit: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.}
}
\value{
A list with components:
\item{best_result_df}{A data frame of best models of mulitple buildings. If a building does not model, it will not be shown in this data frame.}
\item{plot_list}{A list containing plots. If \code{plot_flag} is set to \code{FALSE}, this will be an empty list.}
}
\description{
This function run batches given given a utility data frame of \strong{retrofit data} and energy. See \code{\link{retrofit_utility}.}
}
\examples{
\dontrun{
batch_result = batch_run_energy_retrofit(retrofit_utility, 'Elec')
}
}
