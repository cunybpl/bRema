#' Calculates and graphs parameter based models for retrofit data
#' 
#' This function returns parameters, stats, change-point(s), and parameter model graphs using \code{run_model} function. Do not use this function for unretrofit data.
#' If input \code{util} data frame is not in the exact format as \code{\link{unretrofit_utility}}, change independent varaible column to 'OAT' and dependent variable to 'usage', add 'energy_type' column and set all the values to either 'Fuel' or 'Elec'. If 'estimated' is column is not present, a column of ones will be added automatically.
#' @param util A data frame with columns: energy_type, OAT, usage, prepost (1 for pre-retrotit, 2 for retrofit and 3 for post-retrofit) and estimated (1 for estimated usage and 0 for actual usage). See \code{\link{retrofit_utility}} for more information about data format.
#' @param plot_flag A boolean value. Defaults to \code{FALSE}. If set to \code{FALSE}, it will not return any plots.
#' @param step A single numeric value or a vector. If it is a single numeric value, \code{create_model} will be used to find change-point(s). If it is a vector with more than one component, \code{create_model_2} will be used. Defaults to 0.5. Adjust \code{step}-size depending on independent varaiables.
#' @param n_pre A numeric value that determines threshold for population test for pre-retrofit: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.
#' @param n_post A numeric value that determines threshold for population test for post-retrofit: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.
#' @export
#' @seealso \code{\link{run_model}} and \code{\link{retrofit_utility}}
#' @examples
#' \dontrun{
#' retrofit_util = subset(retrofit_utility, retrofit_utility$bdbid == 846152
#'            & retrofit_utility$energy_type == 'Elec') #retrofit data
#' retrofit_result = run_model_retrofit(retrofit_util)}
run_model_retrofit <- function(util, plot_flag = FALSE, step = 0.5, n_pre = 4, n_post = 4)
{
  pre_util = subset(util, util$prepost == 1)
  post_util = subset(util, util$prepost == 3)
  return(list(pre = run_model(pre_util, plot_flag = plot_flag, step = step, n_pre),
          post = run_model(post_util, plot_flag = plot_flag, step = step, n_post)))
}

#' Batch run for both energy type (retrofit)
#'
#' This function run batches given a utility data frame of \strong{retrofit data}. See \code{\link{retrofit_utility}.}
#' @param utility A utility data frame of retrofitted data with multiple buildings.
#' @param metric_flag A boolean value. Defaults to \code{TRUE}. If set to \code{TRUE}, \code{\link{order_models}} function will be use for scoring models for 'Elec' and \code{\link{inverse_order_models}} for 'Fuel'. Else, \code{inverse_order_models} function will be used for 'Elec' and \code{order_models} for 'Fuel'.
#' @param plot_flag A boolean value. Defaults to \code{FALSE}. If set to \code{FALSE}, it will not return any plots.
#' @param step A single numeric value or a vector. If it is a single numeric value, \code{create_model} will be used to find change-point(s). If it is a vector with more than one component, \code{create_model_2} will be used. Defaults to 0.5. Adjust \code{step}-size depending on independent varaiables.
#' @param CV_RMSE_elec A numeric value. CV-RMSE threshold value for Elec. Defaults to 25.
#' @param Rsquared_elec A numeric value. Rsquared threshold value for Elec. Defaults to 0.75.
#' @param CV_RMSE_fuel A numeric value. CV-RMSE threshold value for Fuel. Defaults to 50.
#' @param Rsquared_fuel A numeric value. Rsquared threshold value for Fuel. Defaults to 0.75.
#' @param n_pre A numeric value that determines threshold for population test for pre-retrofit: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.
#' @param n_post A numeric value that determines threshold for population test for post-retrofit: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.
#' @return A list with components:
#' \item{best_result_df}{A data frame of best models of mulitple buildings. If a building does not model, it will not be shown in this data frame.}
#' \item{plot_list}{A list containing plots. If \code{plot_flag} is set to \code{FALSE}, this will be an empty list.}
#' @export
#' @examples
#' \dontrun{
#' batch_result = batch_run_retrofit(retrofit_utility)
#' }
batch_run_retrofit <- function(utility, metric_flag = TRUE, plot_flag = FALSE, step = 0.5,
            CV_RMSE_elec = 25, Rsquared_elec = 0.75, CV_RMSE_fuel = 50, 
            Rsquared_fuel = 0.75, n_pre = 4, n_post = 4)
{ 
  result = list(best_result_df = data.frame(), plot_list = list())
  for (energy in c('Elec', 'Fuel'))
  {
    temp = batch_run_energy_retrofit(utility, energy, metric_flag = metric_flag,
                plot_flag = plot_flag, step = step, CV_RMSE_elec,
                Rsquared_elec,CV_RMSE_fuel, Rsquared_fuel, n_pre, n_post)

    result$best_result_df = rbind(result$best_result_df,temp$best_result_df)
    result$plot_list = append(result$plot_list, temp$plot_list)
  }
  result$best_result_df = result$best_result_df[order(result$best_result_df[,'bdbid'],
                                  result$best_result_df[,'energy_type']),]
  rownames(result$best_result_df) = c(1:nrow(result$best_result_df))
  return(result)
}


#' Batch run for specific energy (retrofit)
#'
#' This function run batches given given a utility data frame of \strong{retrofit data} and energy. See \code{\link{retrofit_utility}.}
#' @param utility A utility data frame of retrofitted data with multiple buildings.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @param metric_flag A boolean value. Defaults to \code{TRUE}. If set to \code{TRUE}, \code{\link{order_models}} function will be use for scoring models for 'Elec' and \code{\link{inverse_order_models}} for 'Fuel'. Else, \code{inverse_order_models} function will be used for 'Elec' and \code{order_models} for 'Fuel'.
#' @param plot_flag A boolean value. Defaults to \code{FALSE}. If set to \code{FALSE}, it will not return any plots.
#' @param step A single numeric value or a vector. If it is a single numeric value, \code{create_model} will be used to find change-point(s). If it is a vector with more than one component, \code{create_model_2} will be used. Defaults to 0.5. Adjust \code{step}-size depending on independent varaiables.
#' @param CV_RMSE_elec A numeric value. CV-RMSE threshold value for Elec. Defaults to 25.
#' @param Rsquared_elec A numeric value. Rsquared threshold value for Elec. Defaults to 0.75.
#' @param CV_RMSE_fuel A numeric value. CV-RMSE threshold value for Fuel. Defaults to 50.
#' @param Rsquared_fuel A numeric value. Rsquared threshold value for Fuel. Defaults to 0.75.
#' @param n_pre A numeric value that determines threshold for population test for pre-retrofit: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.
#' @param n_post A numeric value that determines threshold for population test for post-retrofit: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.
#' @return A list with components:
#' \item{best_result_df}{A data frame of best models of mulitple buildings. If a building does not model, it will not be shown in this data frame.}
#' \item{plot_list}{A list containing plots. If \code{plot_flag} is set to \code{FALSE}, this will be an empty list.}
#' @export
#' @examples
#' \dontrun{
#' batch_result = batch_run_energy_retrofit(retrofit_utility, 'Elec')
#' }
batch_run_energy_retrofit <- function(utility, energy, metric_flag = TRUE, plot_flag = FALSE,
                                      step = 0.5, CV_RMSE_elec = 25, Rsquared_elec = 0.75,
                                      CV_RMSE_fuel = 50, Rsquared_fuel = 0.75, n_pre = 4, n_post = 4)
{
  utility = subset(utility, utility$energy_type == energy)
  plot_list = list()
  best_result_df = data.frame()

  fiscal_year = max(unique(utility$fiscal_year))
  for (bdbid_n in unique(utility$bdbid))
  {
    util = subset(utility, utility$bdbid == bdbid_n)
    if(check_zeros(util)){next}
    best_result = main_best_model_retrofit(run_model_retrofit(util, plot_flag = plot_flag,
                                              step = step, n_pre, n_post), energy, metric_flag)
    if (!is.null(best_result$pre) & !is.null(best_result$post))
    {
      if (plot_flag)
      {
      	plot_list[[energy]][[as.character(bdbid_n)]][['pre']] = best_result$pre$figure
        plot_list[[energy]][[as.character(bdbid_n)]][['post']] = best_result$post$figure
      }
      temp_pre = construct_model_table(best_result$pre, bdbid_n, energy, fiscal_year,
                                    nrow(util), prepost = 1)
      temp_post = construct_model_table(best_result$post, bdbid_n, energy, fiscal_year,
                                    nrow(util), prepost = 3)
      best_result_df = rbind(best_result_df, temp_pre, temp_post)
    }
  }
  return(list(best_result_df = best_result_df, plot_list = plot_list))
}

#' Returns the best model for retrofit data
#' 
#' This function returns the best model if there is one for retrofit data. Otherwise, return a null list.
#' @details
#' If some models pass all of shape test, t-test, and population test but do not meet threshold CV-RMSE, the model will the lowest CV-RMSE will be selected as the best model. If none of the model passes all of shape test, t-test, and population test, it will return a list with null component.
#' @param inter_result A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param energy A charcter string. Energy Type, either 'Elec' or 'Fuel'.
#' @param metric_flag A boolean value. Defaults to TRUE. If set to \code{TRUE}, \code{\link{order_models}} function will be use for scoring models for 'Elec' and \code{\link{inverse_order_models}} for 'Fuel'. Else, \code{inverse_order_models} function will be used for 'Elec' and \code{order_models} for 'Fuel'.
#' @param CV_RMSE_elec A numeric value. CV-RMSE threshold value for Elec. Defaults to 25.
#' @param Rsquared_elec A numeric value. Rsquared threshold value for Elec. Defaults to 0.75.
#' @param CV_RMSE_fuel A numeric value. CV-RMSE threshold value for Fuel. Defaults to 50.
#' @param Rsquared_fuel A numeric value. Rsquared threshold value for Fuel. Defaults to 0.75.
#' @export
#' @seealso \code{\link{main_best_model_func}}, \code{\link{run_model}}, and \code{\link{run_model_retrofit}}
#' @examples
#' \dontrun{
#' retrofit_util = subset(retrofit_utility, retrofit_utility$bdbid == 846152
#'            & retrofit_utility$energy_type == 'Elec') #retrofit data
#' retrofit_result = run_model_retrofit(retrofit_util)
#' retrofit_best_result = main_best_model_retrofit(retrofit_result, 'Elec')}
main_best_model_retrofit <- function(inter_result, energy, metric_flag = TRUE,
                            CV_RMSE_elec = 25, Rsquared_elec = 0.75, CV_RMSE_fuel = 50, 
                            Rsquared_fuel = 0.75)
{ 

  pre_result = inter_result$pre
  post_result = inter_result$post

  pre_best = main_best_model_func(pre_result, energy, metric_flag,
                              CV_RMSE_elec, Rsquared_elec, CV_RMSE_fuel, Rsquared_fuel)
  post_best = main_best_model_func(post_result, energy, metric_flag,
                              CV_RMSE_elec, Rsquared_elec, CV_RMSE_fuel, Rsquared_fuel)

  if (length(pre_best) == 0)
  {
  	pre_best = best_worst_model(pre_result, NA, energy, NA, NA, 'pre')
  }

  if (length(post_best) == 0)
  {
    post_best = best_worst_model(post_result, NA, energy, NA, NA, 'post')   
  }

  return(list(pre = pre_best, post = post_best))
}


#' Construct table for all models
#'
#' This functions returns a data frame given a nested list of model result.
#'
#' @param inter_result A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param bdbid Building id, which identifies the building or the dataset. Defaults to \code{NA}.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @param fiscal_year Fiscal year. Defaults to \code{NA}.
#' @param n number of observed points (independent variable) in utility data frame of a building. Defaults to \code{NA}.
#' @param prepost A numeric value. Defaults to be 1. This indicates if it is unretrofit or retrofit. See \code{\link{unretrofit_utility}} for more information about prepost.
#' @export
#' @examples
#' \dontrun{
#' retrofit_util = subset(retrofit_utility, retrofit_utility$bdbid == 846152
#'            & retrofit_utility$energy_type == 'Elec') #retrofit data
#' retrofit_result = run_model_retrofit(retrofit_util)
#' pre_best_df = construct_all_model(retrofit_result$pre, bdbid = 846152, energy = 'Elec')}
construct_all_model <- function(inter_result, bdbid = NA, energy, fiscal_year = NA, n = NA, prepost = 1)
{	
	df = data.frame()
	for (model in c('2P','3PC','3PH','4P','5P'))
	{	
		temp = construct_model_table(inter_result[[model]],
					bdbid, energy, fiscal_year, n, prepost)
		df = rbind(df,temp)
	}
	return(df)
}


#' Filters model with lowest CV-RMSE
#'
#' This function filters for model with lowest CV-RMSE value given a nested list of model result.
#' @details
#' If some models pass all of shape test, t-test, and population test but do not meet threshold CV-RMSE, the model will the lowest CV-RMSE will be selected as the best model. If none of the model passes all of shape test, t-test, and population test, it will return \code{NULL}.
#' @param inter_result A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param bdbid Building id, which identifies the building or the dataset. Defaults to \code{NA}.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @param fiscal_year Fiscal year. Defaults to \code{NA}.
#' @param n number of observed points (independent variable) in utility data frame of a building. Defaults to \code{NA}.
#' @param prepost A numeric value. Defaults to be 1. This indicates if it is unretrofit or retrofit (pre or post). See \code{\link{unretrofit_utility}} for more information about prepost.
#' @export
#' @seealso \code{\link{main_best_model_func}}, \code{\link{run_model}}, and \code{\link{run_model_retrofit}}
#' @examples
#' \dontrun{
#' retrofit_util = subset(retrofit_utility, retrofit_utility$bdbid == 846152
#'            & retrofit_utility$energy_type == 'Elec') #retrofit data
#' retrofit_result = run_model_retrofit(retrofit_util)
#' retrofit_best_result = best_worst_model(retrofit_result$post, bdbid = 846152, 'Elec', prepost = 3)}
best_worst_model <- function(inter_result, bdbid = NA, energy, fiscal_year = NA, n = NA, prepost = 1)
{	
	df = construct_all_model(inter_result, bdbid, energy, fiscal_year, n, prepost)
	df = subset(df, df$main_test == 'Pass')

	if (is.null(df))
  {
    return(NULL)
  }else
  {
		df = df[order(df['cv_rmse']), ]
    return(inter_result[[as.character(.subset2(df, 'model_type')[1])]])
  }
}