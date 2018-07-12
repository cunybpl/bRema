#' Calculates savings
#'
#' This function calculates savings for \strong{retrofit data}.
#' @param util A post-retrofit data frame with columns: OAT, usage and end_date. See \code{\link{unretrofit_utility}} for more information about data format.
#' @param best_df Pre retrofit best model data frame (of a single builiding) returned from \code{\link{batch_run}}.
#' @param bdbid Building id, which identifies the building or the dataset. Defaults to \code{NA}.
#' @param energy A charcter string. Energy Type, either 'Elec' or 'Fuel'.
#' @param session_id Either a character string or a numeric value to keep track. Defaults to \code{NA}.
#' @export
#' @examples
#' \dontrun{
#' retrofit_util = subset(retrofit_utility, retrofit_utility$bdbid == 846152
#'					& retrofit_utility$energy_type == 'Elec')
#' result = batch_run_energy_retrofit(retrofit_util, 'Elec')
#' pre_retrofit_util = subset(retrofit_util, retrofit_util$prepost == 3)
#' #subsetting for pre-retrofit period
#' pre_result = subset(result$best_result_df, result$best_result_df$prepost == 1)
#' savings_df = saving_calc(pre_retrofit_util, pre_result, 846152, 'Elec')
#' }
saving_calc <- function(util, best_df, bdbid = NA, energy, session_id = NA)
{
  options(digits=15)
  util$end_date = strptime(util$end_date, format = "%Y-%m-%d")
  util$day = as.numeric(format(util$end_date, format = "%d"))

  B = c(.subset2(best_df, 'ycp')[1],
  			.subset2(best_df, 'ls')[1], .subset2(best_df, 'rs')[1])
  model = .subset2(best_df, 'model_type')[1]
  cp1 = .subset2(best_df, 'xcp1')[1]
  cp2 = .subset2(best_df, 'xcp2')[1]

  adj_df = y_gen(as.character(model), util$OAT, B, cp1, cp2)
  total_adj_usage = sum(adj_df*util$day)

  total_post_usage = sum(util$usage*util$day)
  percent_savings = 1 - total_post_usage/total_adj_usage
  yearly_savings = total_adj_usage - total_post_usage

  calc_type = c('percent_savings', 'total_adj_baseline_usage', 'total_post_usage', 'yearly_savings')
  saving_df = data.frame(bdbid = bdbid, calc_type = calc_type, energy_type = energy, session_id = session_id)
  saving_df$savings = c(percent_savings,total_adj_usage,total_post_usage,yearly_savings)
  return(saving_df)
}


#' Handles calculation for savings and savings uncertainty
#'
#' This function handles calculation for savings and savings uncertainty for \strong{retrofit data}.
#' @param utility A utility data frame of \strong{retrofit data} with multiple buildings.
#' @param best_model best_model A data frame returns from \code{\link{batch_run}} or \code{\link{batch_run_energy}}
#' @param session_id Either a character string or a numeric value to keep track. Defaults to \code{NA}.
#' @return A list with components:
#' \item{saving_df}{A data frame of saving calculation}
#' \item{uncertainty_df}{A data frame of saving uncertainty calculation}
#' @export
#' @examples
#' \dontrun{
#' batch = batch_run_retrofit(retrofit_utility)
#' saving_list = main_saving_calc(retrofit_utility, batch$best_result_df)
#' }
main_saving_calc <- function(utility, best_model, session_id = NA)
{
	saving_df = data.frame()
	uncertainty_df = data.frame()
	for (energy in c('Elec', 'Fuel'))
	{	
		best_df_energy = subset(best_model, best_model$energy_type == energy)
		for (bdbid_n in unique(best_df_energy$bdbid))
		{	
			pre_util = subset(utility, utility$bdbid == bdbid_n & utility$energy_type == energy &
								utility$prepost == 1)
			post_util = subset(utility, utility$bdbid == bdbid_n & utility$energy_type == energy &
								utility$prepost == 3)

			best_df = subset(best_df_energy, best_df_energy$bdbid == bdbid_n
										& best_df_energy$prepost  == 1)
			temp = saving_calc(post_util, best_df, bdbid_n, energy, session_id =NA)

			rmse = .subset2(best_df, 'rmse')[1]
			yearly_savings = .subset2(temp, 'savings')[4]

			saving_df = rbind(saving_df, temp)
			temp = saving_uncertainty_calc(nrow(pre_util), nrow(post_util), rmse, yearly_savings,
						energy, bdbid_n, session_id)
			uncertainty_df = rbind(uncertainty_df, temp)
		}
	}
	saving_df = saving_df[order(saving_df[,'bdbid'], saving_df[,'energy_type']),]
	uncertainty_df = uncertainty_df[order(uncertainty_df[,'bdbid'],
							uncertainty_df[,'energy_type']),]
	rownames(saving_df) = c(1:nrow(saving_df))
	rownames(uncertainty_df) = c(1:nrow(uncertainty_df))
	return(list(saving_df = saving_df, uncertainty_df = uncertainty_df))
}


#' Calculates saving uncertainity
#'
#' This function calculates saving uncertainty for \strong{retrofit data}.
#' @param num_pre A numeric value. Number of pre retrofit data points.
#' @param num_post A numeric value. Number of post retrofit data points.
#' @param rmse A numeric value. Pre-retrofit RMSE-value.
#' @param yearly_savings A numeric value. Yearly savings returned from \code{\link{saving_calc}}.
#' @param energy A charcter string. Energy Type, either 'Elec' or 'Fuel'.
#' @param bdbid Building id, which identifies the building or the dataset. Defaults to \code{NA}.
#' @param session_id Either a character string or a numeric value to keep track. Defaults to \code{NA}.
#' @export
#' @examples
#' \dontrun{
#' retrofit_util = subset(retrofit_utility, retrofit_utility$bdbid == 846152
#'					& retrofit_utility$energy_type == 'Elec')
#' result = batch_run_energy_retrofit(retrofit_util, 'Elec')
#' 
#' #subsetting for pre-retrofit period
#' pre_result = subset(result$best_result_df, result$best_result_df$prepost == 1)
#' pre_retrofit_util = subset(retrofit_util, retrofit_util$prepost == 3)
#' savings_df = saving_calc(pre_retrofit_util, pre_result, 846152, 'Elec')
#'	
#' rmse = .subset2(pre_result, 'rmse')[1]
#' yearly_savings = .subset2(savings_df, 'savings')[4]
#' num_pre = nrow(subset(retrofit_util, retrofit_util$prepost == 1))
#' num_post = nrow(subset(retrofit_util, retrofit_util$prepost == 3))
#' uncertainty_df = saving_uncertainty_calc(num_pre, num_post, rmse, yearly_savings,
#'											'Elec', 846152)
#' }
saving_uncertainty_calc <- function(num_pre, num_post, rmse, yearly_savings, energy,
										bdbid = NA, session_id = NA)
{
	r = sqrt(num_post * (1.0 + (2.0/num_pre)))
	savings_uncertainty = 1.96*(r*rmse)

	c_year = yearly_savings/num_post

	psu = abs(savings_uncertainty)/abs(c_year)
	moe = abs(psu*yearly_savings)

	calc_type = c('moe_yr', 'psu')
	df = data.frame(bdbid = bdbid, calc_type = calc_type, energy_type = energy,
		savings_uncertainty = c(moe, psu), session_id = session_id)
	return(df)
}
