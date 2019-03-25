#' Post model calculations
#'
#' This function transforms best_model in a more meaningful way and returns calculation results after modeller, such as numeric and percent ranks for heating/cooling change-points, heating/cooling sensitivity and baseload.
#' @details If \code{best_model} data frame contains only one energy type, \code{\link{post_model_energy}} can be used instead.
#' @param utility A data frame of \strong{unretrofit data} with multiple buildings, with colums: OAT, usage, energy_type and end_date. See \code{\link{unretrofit_utility}} for data format.
#' @param best_model A data frame returns from \code{\link{batch_run}} or \code{\link{batch_run_energy}}.
#' @param rank_flag A boolean value. Defaults to \code{FALSE}. If set to \code{TRUE}, \code{\link{lean_analysis_ranking}} function will be called and lean analysis ranking will be performed and added to resulted dataframe from \code{main_post_model}.
#' @export
#' @examples
#' \dontrun{
#' batch_result = batch_run(unretrofit_utility)
#' post_df = main_post_model(unretrofit_utility, batch_result$best_result_df)}
main_post_model <- function(utility, best_model, rank_flag = FALSE)
{
  options(digits=15)

  if(isTRUE(is.null(utility$end_date))){
    stop("end_date column is missing from argument utility.", call. = FALSE)
  }

  if(isTRUE(is.na(utility$end_date))){
    stop("end_date column of argument utility has missing value(s).", call. = FALSE)
  }

  best_model$energy_type = as.character(best_model$energy_type)
  best_model$model_type = as.character(best_model$model_type)

  post_df = data.frame()
  for (energy in unique(best_model$energy_type))
  { 
    temp = post_model_energy(utility, best_model, energy, rank_flag)
    post_df = rbind(post_df, temp)
  }

  post_df = post_df[order(post_df[,'bdbid'], post_df[,'energy_type']),]
  rownames(post_df) = c(1:nrow(post_df))

  return(post_df)
}

#' Post model calculations for specific energy type
#'
#' This function transforms best_model in a more meaningful way and returns lean analysis calculation results after modeller, such as numeric and percent ranks for heating/cooling change-points, heating/cooling sensitivity and baseload.
#' @param utility A data frame of \strong{unretrofit data} with multiple buildings, with colums: OAT, usage, energy_type and end_date. See \code{\link{unretrofit_utility}} for data format.
#' @param best_model A data frame returns from \code{\link{batch_run}} or \code{\link{batch_run_energy}}.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @param rank_flag A boolean value. Defaults to \code{FALSE}. If set to \code{TRUE}, \code{\link{lean_analysis_ranking}} function will be called and lean analysis ranking will be performed and lean analysis ranking will be performed and added to resulted dataframe from \code{post_model_energy}.
#' @export
#' @examples
#' \dontrun{
#' batch_result = batch_run(unretrofit_utility)
#' post_df_elec = post_model_energy(unretrofit_utility, batch_result$best_result_df, 'Elec')}
post_model_energy <- function(utility, best_model, energy, rank_flag = FALSE)
{ 
  options(digits=15)
  post_df = data.frame()

  if(is.factor(.subset2(best_model, 'model_type')[1]))
  {
    best_model$energy_type = as.character(best_model$energy_type)
    best_model$model_type = as.character(best_model$model_type)
  }

  utility = subset(utility, utility$energy_type == energy)
  best_model = subset(best_model, best_model$energy_type == energy)

  for (bdbid_n in unique(best_model$bdbid))
  { 
    util = subset(utility, utility$bdbid == bdbid_n)
    best_df = subset(best_model, best_model$bdbid == bdbid_n)
    temp = post_model_preliminary(util, best_df, bdbid_n, energy)
    #temp = cbind(temp, percent_heat_cool_func(temp$total_consumption, temp$heat_load, temp$cool_load))
    post_df = rbind(post_df, temp)
  }
  post_df = cbind(post_df,
                  percent_heat_cool_func(post_df$predicted_total_consumption,
                                        post_df$predicted_heat_load,
                                        post_df$predicted_cool_load))

  load_df = calc_heat_cool_load(post_df$percent_heating, post_df$percent_cooling,
                          post_df$percent_baseload, post_df$total_consumption)
  post_df = post_df[, !(colnames(post_df) %in% c('predicted_total_consumption', 'predicted_heat_load', 'predicted_cool_load'))]
  post_df = cbind(post_df, load_df)

  if (rank_flag)
  { 
    lean_df = lean_analysis_ranking(post_df, energy)
    lean_df = lean_df[,4:ncol(lean_df)]
    post_df = cbind(post_df, lean_df)
  }
  return(post_df)
}

#' Performs preliminary post model calculation and rearranging 
#'
#' This function transforms \code{best_model} in a more meaningful way and returns calculations such as predicted heating load and predicted cooling load, which will be used to calculated percent heating, cooling and baseload.
#' @param util A data frame with columns: OAT, usage, and end_date. See \code{\link{unretrofit_utility}} for more information about data format.
#' @param best_df Best model data frame (of a single builiding) returned from \code{\link{batch_run}}.
#' @param bdbid_n Building id, which identifies the building or the dataset. Defaults to \code{NA}.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @export
#' @import lubridate
#' @examples
#' \dontrun{
#' bdbid_n = 'f3acce86'
#' energy = 'Elec'
#' #first subsetting utility data frame by bdbid and energy
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86' &
#'                unretrofit_utility$energy_type == 'Elec')
#' #finding best model and calculating best model data frame
#' best_df = batch_run_energy(util, 'Elec')$best_result_df
#' post_df = post_model_preliminary(util, best_df, bdbid_n, energy)
#' }
post_model_preliminary <- function(util, best_df, bdbid_n = NA, energy)
{ 
  options(digits=15)

  if(!is.POSIXlt(util$end_date) & !is.POSIXt(util$end_date) & !is.POSIXct(util$end_date))
  { 
    if (grepl('/', util$end_date[1]))
    {
      util$end_date = strptime(util$end_date, format = "%m/%d/%y")
    }else
    {
      util$end_date = strptime(util$end_date, format = "%Y-%m-%d")
    }
  }
  
  util$day = as.numeric(format(util$end_date, format = "%d"))

  df = data.frame(bdbid = bdbid_n, energy_type = energy)

  df$period = .subset2(best_df, 'n')[1]
  df$model_type = .subset2(best_df, 'model_type')[1]

  df$ycp= .subset2(best_df, 'ycp')[1]

  df = cbind(df, heat_cool_sen_change(best_df, .subset2(df, 'model_type')[1],
                energy))

  df$total_consumption = sum(util$usage * util$day)

  util$predicted_usage = calc_predicted_usage(util, best_df)
  df$predicted_total_consumption = sum(util$predicted_usage * util$day)

  if (.subset2(df,'model_type')[1] == '2P'){
    return(cbind(df, predict_heat_cool_load_2P(util,
            .subset2(df, 'predicted_total_consumption')[1], .subset2(best_df, 'ls')[1])))
  }

  df = cbind(df, predict_heat_cool_load_func(util, .subset2(df,'model_type')[1],
                .subset2(df,'ycp')[1], .subset2(df, 'heating_change_point')[1],
                .subset2(df,'cooling_change_point')[1]))

  #df = cbind(df, percent_heat_cool_func(.subset2(df, 'total_consumption')[1],
  #             .subset2(df, 'heat_load')[1], .subset2(df, 'cool_load')[1]))

  return(df)
}

#' Extract heating/cooling sensitivity and change-points
#' 
#' This function extracts slopes and change-points from \code{best_model} data frame and renames them meaningfully depending on model. For example, if it is '3PC' model, right slope (rs) becomes cooling senitivity. 
#' @param best_df Best model data frame (of a single builiding) returned from \code{\link{batch_run}}.
#' @param model A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'. The model must be present in \code{best_df}.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @export
#' @examples 
#' \dontrun{
#' best_df_'f3acce86' = subset(best_df, best_df$bdbid == 'f3acce86' & best_df$energy_type == 'Elec')
#' model = as.character(best_df_'f3acce86'$model_type)
#' heat_cool_sen_change(best_df_'f3acce86', model, 'Elec')
#' #compare the result with best_df_'f3acce86' to see what have been renamed.
#' }
heat_cool_sen_change <- function(best_df, model, energy)
{ 
  options(digits=15)
  df = data.frame(heating_change_point  = switch(model,
                "3PC" = .subset2(best_df, 'xcp1')[1], "3PH" = .subset2(best_df, 'xcp1')[1],
                "4P" = .subset2(best_df, 'xcp1')[1],
                "5P" = .subset2(best_df, 'xcp1')[1], NA)) #set zero or NULL later for elec

  df$cooling_change_point = switch(model,
                "3PC" = .subset2(best_df, 'xcp1')[1], "3PH" = .subset2(best_df, 'xcp1')[1],
                "4P" = .subset2(best_df, 'xcp1')[1],
                "5P" = .subset2(best_df, 'xcp2')[1], NA)

  df$heating_sensitivity  = switch(model,
                "3PH" = .subset2(best_df, 'ls')[1],
                "4P" = .subset2(best_df, 'ls')[1],
                "5P" = .subset2(best_df, 'ls')[1], "3PC" = 0,
                "2P" = ifelse(.subset2(best_df, 'ls')[1] < 0, .subset2(best_df, 'ls')[1], 0))


  df$cooling_sensitivity = switch(model,
                "3PC" = .subset2(best_df, 'rs')[1],
                "4P" = .subset2(best_df, 'rs')[1],
                "5P" = .subset2(best_df, 'rs')[1], "3PH" = 0,
                "2P" = ifelse(.subset2(best_df, 'ls')[1] > 0, .subset2(best_df, 'ls')[1], 0))

  return(df)
}

#' Calculates percentanges of heat/cool load and baseload
#'
#' This function calculates percentanges of heat/cool load and baseload by using output from \code{\link{predict_heat_cool_load_func}}.
#' @param predicted_total_consumption total predicted consumption. To calculate predicted_total_consumption, use output returned from \code{\link{calc_predicted_usage}}.
#' @param predicted_heat_load predicted heating load. Use output return from \code{\link{calc_predict_heat_load}} or \code{\link{predict_heat_cool_load_func}}.
#' @param predicted_cool_load preidcted cooling load. Use output return from \code{\link{calc_predict_cool_load}} or \code{\link{predict_heat_cool_load_func}}.
#' @export
#' @examples
#' bdbid_n = 'f3acce86'
#' energy = 'Elec'
#' #first subsetting utility data frame by bdbid and energy
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86' &
#'                unretrofit_utility$energy_type == 'Elec')
#' #finding best model and calculating best model data frame
#' best_df = batch_run_energy(util, 'Elec')$best_result_df
#' post_df = post_model_preliminary(util, best_df, bdbid_n, energy)
#' percent_df = percent_heat_cool_func(post_df$predicted_total_consumption,
#'                                      post_df$predicted_heat_load, post_df$predicted_cool_load)
percent_heat_cool_func <- function(predicted_total_consumption, predicted_heat_load, predicted_cool_load)
{ 
  options(digits=15) 
  df = data.frame(percent_cooling = predicted_cool_load/predicted_total_consumption,
      percent_heating = predicted_heat_load/predicted_total_consumption)
  df$percent_baseload = 1 - df$percent_cooling - df$percent_heating
  return(df)
}

#' Calculates heating and cooling load
#'
#' This function calculates predicted heating and cooling load of a builiding using predicted usage.
#' @details For '2P' model, see \code{\link{predict_heat_cool_load_2P}}.
#' @param utility A utility data frame of \strong{unretrofit data} with columns: energy_type, OAT, usage, end_date, day and predicted_usage. Day column: the end date from end_date column.
#' @param model A character string. Model: '3PH', '3PC', '4P' or '5P'.
#' @param ycp y-value at \code{cp1} and \code{cp2}.
#' @param cp1 A numeric value. The first change-point. Defaults to NA.
#' @param cp2 A numeric value. The second change-point. Defaults to NA.
#' @export
#' @examples
#' \dontrun{
#' bdbid_n = 'f3acce86'
#' energy = 'Elec'
#' #first subsetting utility data frame by bdbid and energy
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86' &
#'                unretrofit_utility$energy_type == 'Elec')
#' #finding best model and calculating best model data frame
#' best_df = batch_run_energy(util, 'Elec')$best_result_df 
#' util$predicted_usage = calc_predicted_usage(util, best_df)
#' predicted_load_df = predict_heat_cool_load_func(util, best_df$model,
#'                              best_df$ycp, best_df$cp1, best_df$cp2)
#' }
predict_heat_cool_load_func <- function(utility, model, ycp, cp1 = NA, cp2 = NA)
{ 
  options(digits=15)

  heat_load = switch(model, "5P" = calc_predict_heat_load(utility, cp1, ycp),
                    "4P" = calc_predict_heat_load(utility, cp1, ycp),
                    "3PH" = calc_predict_heat_load(utility, cp1, ycp), 
                    "3PC" = 0)

  cool_load = switch(model, "5P" = calc_predict_cool_load(utility, cp2, ycp),
                    "4P" = calc_predict_cool_load(utility, cp2, ycp),
                    "3PC" = calc_predict_cool_load(utility, cp2, ycp), 
                    "3PH" = 0)

  return(data.frame(predicted_heat_load = heat_load, predicted_cool_load = cool_load))
}

#' Calculates predicted heating load
#'
#' This function calculates heating load by using predicted usage.
#' @param utility A utility data frame with columns: energy_type, OAT, usage, end_date, day and predicted_usage. Day column: the end date from end_date column. predicted_usage column: output from \code{\link{calc_predicted_usage}}.
#' @param cp heating change-point.
#' @param ycp y-values at \code{cp}
#' @export
calc_predict_heat_load <- function(utility, cp, ycp)
{ 
  options(digits=15)
  df = subset(utility, utility$OAT < cp)
  df$heat_load = (df$predicted_usage - ycp)*df$day
  df$heat_load[df$heat_load < 0] <- 0 
  total_heat_load  = sum(df$heat_load)
  return(total_heat_load)
}

#' Calculates predicted cooling load
#' 
#' This function calculates cooling load by using predicted usage.
#' @param utility A utility data frame with columns: energy_type, OAT, usage, end_date, day and predicted_usage. Day column: the end date from end_date column. predicted_usage column: output from \code{\link{calc_predicted_usage}}.
#' @param cp cooling change-point.
#' @param ycp y-values at \code{cp}
#' @export
calc_predict_cool_load <- function(utility, cp, ycp)
{
  options(digits=15)
  df = subset(utility, utility$OAT > cp)
  df$cool_load = (df$predicted_usage - ycp)*df$day
  df$cool_load[df$cool_load < 0] <- 0 
  total_cool_load  = sum(df$cool_load)
  return(total_cool_load)
}

#' Ranks baseload, heating/cooling change-points and sensitivity.
#'
#' This function calculates numeric and percent rank for baseload, heating/cooling change-points and sensitivity.
#' @param post_df A data frame returned from \code{\link{post_model_preliminary}}. The data frame should include multiple buildings to have meaningful interpretation of the rank.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @export
#' @examples
#' \dontrun{
#' batch_result = batch_run(unretrofit_utility)
#' post_df = main_post_model(unretrofit_utility, batch_result$best_result_df)
#' lean_elec = lean_analysis_ranking(post_df, 'Elec')
#' }
lean_analysis_ranking <- function(post_df, energy)
{ 
  post_df = subset(post_df, post_df$energy_type == energy)

  lean_df = data.frame(bdbid = post_df$bdbid, model_type = post_df$model_type)
  lean_df$energy_type = energy

  lean_name = c('heating_change_point', 'cooling_change_point',
    'heating_sensitivity', 'cooling_sensitivity', 'baseload')

  for (rank_type in lean_name)
  {   
    numeric_name = paste(rank_type,'_numeric_rank', sep = "")
    percent_name = paste(rank_type,'_percent_rank', sep = "")

    temp = subset(post_df, post_df[[rank_type]] != 0)
    if (rank_type == 'heating_sensitivity')
    {
      temp$heating_sensitivity = abs(temp$heating_sensitivity)
    }

    x = rank(temp[[rank_type]])
    temp[[numeric_name]] = x
    temp[[percent_name]] = 100*x/length(x)


    if (rank_type == 'cooling_change_point')
    {
      temp[[numeric_name]] = 1 + length(x) - x 
      temp[[percent_name]] = 100 - temp[[percent_name]]
    }

    lean_df[[numeric_name]] = NA
    lean_df[[percent_name]] = NA

    lean_df[[numeric_name]][lean_df$bdbid %in% temp$bdbid] <- temp[[numeric_name]]
    lean_df[[percent_name]][lean_df$bdbid %in% temp$bdbid] <- temp[[percent_name]]
  }
  return(lean_df)
}

#' Calculates percentanges of heat/cool load and baseload
#'
#' This function calculates predicted usage (of a single building) based on modeller result. The output \code{predicted_usage} is used in \code{\link{predict_heat_cool_load_func}}
#' @param util A data frame with columns: OAT, usage, and end_date. See \code{\link{unretrofit_utility}} for more information about data format.
#' @param best_df Best model data frame (of a single builiding) returned from \code{\link{batch_run}}.
#' @export
#' @examples
#' \dontrun{
#' bdbid_n = 'f3acce86'
#' energy = 'Elec'
#' #first subsetting utility data frame by bdbid and energy
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86' &
#'                unretrofit_utility$energy_type == 'Elec')
#' #finding best model and calculating best model data frame
#' best_df = batch_run_energy(util, 'Elec')$best_result_df
#' util$predicted_usage = calc_predicted_usage(util, best_df)
#' }
calc_predicted_usage <- function(util, best_df){
  temp = c(best_df$ycp, best_df$ls, best_df$rs)
  B = as.matrix(c(temp[1],temp[2:3][temp[2:3] != 0 ]))
  predicted_usage = y_gen(as.character(.subset2(best_df, 'model_type')[1]),
                          util$OAT, B, .subset2(best_df, 'xcp1')[1],
                          cp2 = .subset2(best_df, 'xcp2')[1])
  return(predicted_usage)
}

#' Calculates baseload and heating and cooling load
#'
#' This function calculates \strong{(not predicted)} baseload and heating and cooling load by using output from \code{\link{percent_heat_cool_func}} and total_cosumption.
#' @details This function is different from \code{\link{predict_heat_cool_load_func}}. See Heat and Cool Load, subsection of Methodlogy section of bRema paper.
#' @param percent_heating A numeric vector.
#' @param percent_cooling A numeric vector.
#' @param percent_baseload A numeric vector.
#' @param total_consumption A numeric vector.
#' @export
calc_heat_cool_load <- function(percent_heating, percent_cooling, percent_baseload, total_consumption){
  heat_load = percent_heating*total_consumption
  cool_load = percent_cooling*total_consumption
  baseload = percent_baseload*total_consumption
  return(data.frame(heat_load = heat_load, cool_load = cool_load, baseload = baseload))
}

#' Calculates heating and cooling load for 2P
#'
#' This function calculates predicted heating and cooling load of a builiding for '2P', model type.
#' @details Since 2P does not have any changepoints, heating load and cooling load are calculated differently than other models. See Heat and Cool Load, subsection of Methodlogy section of bRema paper. For other model types, see \code{\link{predict_heat_cool_load_func}}.
#' @param util A utility data frame of \strong{unretrofit data} with columns: energy_type, OAT, usage, end_date, day and predicted_usage. Day column: the end date from end_date column.
#' @param predicted_total_consumption A numeric value. Gross total predicted usage.
#' @param slope A numeric value.
#' @export
#' @examples
#' \dontrun{
#' bdbid_n = 'f3acce86'
#' energy = 'Elec'
#' #first subsetting utility data frame by bdbid and energy
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86' &
#'                unretrofit_utility$energy_type == 'Elec')
#' #finding best model and calculating best model data frame
#' best_df = batch_run_energy(util, 'Elec')$best_result_df 
#' slope = subset(best_df$ls, best_df$bdbid == 'f3acce86' &
#'          best_df$energy_type == 'Elec')
#' util$predicted_usage = calc_predicted_usage(util, best_df)
#' predicted_total_consumption = sum(util$predicted_usage * util$day)
#' predicted_load_df = predict_heat_cool_load_2P(util,
#'            predicted_total_consumption, slope)
#' }
predict_heat_cool_load_2P <- function(util, predicted_total_consumption, slope){
  util = subset(util, min(util$predicted_usage) == util$predicted_usage)
  baseload = util$predicted_usage*util$day
  load =  predicted_total_consumption - baseload

  if (slope < 0){
    return(data.frame(predicted_heat_load = load, predicted_cool_load = 0))
  }else{
    return(data.frame(predicted_heat_load = 0, predicted_cool_load = load))
  }
}