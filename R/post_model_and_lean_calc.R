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

  if(is.factor(.subset2(df, 'model_type')[1]))
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
    temp = post_model_prelimanary(util, best_df, bdbid_n, energy)
    #temp = cbind(temp, percent_heat_cool_func(temp$total_consumption, temp$heat_load, temp$cool_load))
    post_df = rbind(post_df, temp)
  }
  post_df = cbind(post_df, percent_heat_cool_func(post_df$total_consumption, post_df$heat_load, post_df$cool_load))

  if (rank_flag)
  { 
    lean_df = lean_analysis_ranking(post_df, energy)
    lean_df = lean_df[,4:ncol(lean_df)]
    post_df = cbind(post_df, lean_df)
  }
  return(post_df)
}

#' Performs prelimanary post model calculation and rearranging 
#'
#' This function transforms \code{best_model} in a more meaningful way and returns calculations such as heating load and cooling load.
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
#' post_df = post_model_prelimanary(util, best_df, bdbid_n, energy)
#' }
post_model_prelimanary <- function(util, best_df, bdbid_n = NA, energy)
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

  df = cbind(df, main_heat_cool_load(util,
              .subset2(df, 'heating_change_point')[1], .subset2(df,'cooling_change_point')[1],
              .subset2(df,'ycp')[1], .subset2(df,'model_type')[1]))

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
                "5P" = .subset2(best_df, 'xcp1')[1], 0)) #set zero or NULL later for elec

  df$cooling_change_point = switch(model,
                "3PC" = .subset2(best_df, 'xcp1')[1], "3PH" = .subset2(best_df, 'xcp1')[1],
                "4P" = .subset2(best_df, 'xcp1')[1],
                "5P" = .subset2(best_df, 'xcp2')[1], 0)

  df$heating_sensitivity  = switch(model,
                "3PH" = .subset2(best_df, 'ls')[1],
                "4P" = .subset2(best_df, 'ls')[1],
                "5P" = .subset2(best_df, 'ls')[1], "3PC" = 0,
                "2P" = switch(energy, "Elec" = 0,
                  .subset2(best_df, 'ls')[1]))

  df$cooling_sensitivity = switch(model,
                "3PC" = .subset2(best_df, 'rs')[1],
                "4P" = .subset2(best_df, 'rs')[1],
                "5P" = .subset2(best_df, 'rs')[1], "3PH" = 0,
                "2P" = switch(energy, "Fuel" = 0,
                  .subset2(best_df, 'ls')[1]))

  return(df)
}

#' Calculates percentanges of heat/cool load and baseload
#'
#' This function calculates baseload and percentanges of heat/cool load and baseload.
#' @param consumption total consumption.
#' @param heat_load heating load.
#' @param cool_load cooling load.
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
#' post_df = post_model_prelimanary(util, best_df, bdbid_n, energy)
#' percent_heat_cool_func(post_df$total_consumption, post_df$heat_load, post_df$cool_load)
#' }
percent_heat_cool_func <- function(consumption, heat_load, cool_load)
{ 
  options(digits=15)
  df = data.frame(percent_cooling = cool_load/consumption,
      percent_heating = heat_load/consumption)
  df$percent_baseload = 1 - df$percent_cooling - df$percent_heating
  df$baseload = consumption*df$percent_baseload
  return(df)
}

#' Calculates heating and cooling load
#'
#' This function calculates heating and cooling load of a builiding. 
#' @param utility A utility data frame of \strong{unretrofit data} with columns: energy_type, OAT, usage, end_date and day. Day column: the end date from end_date column.
#' @param cp1 A numeric value. The first change-point. Defaults to 0.
#' @param cp2 A numeric value. The second change-point. Defaults to 0.
#' @param ycp y-value at \code{cp1} and \code{cp2}.
#' @param model A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.
#' @export
main_heat_cool_load <- function(utility, cp1 = 0, cp2 = 0, ycp, model)
{ 
  options(digits=15)
  heat_load = switch(model, "5P" = calc_heat_load(utility, cp1, ycp),
                    "4P" = calc_heat_load(utility, cp1, ycp),
                    "3PH" = calc_heat_load(utility, cp1, ycp), 
                    "3PC" = 0, "2P" = 0)

  cool_load = switch(model, "5P" = calc_cool_load(utility, cp2, ycp),
                    "4P" = calc_cool_load(utility, cp2, ycp),
                    "3PC" = calc_cool_load(utility, cp2, ycp), 
                    "3PH" = 0, "2P" = 0)

  return(data.frame(heat_load = heat_load, cool_load = cool_load))
}

#' Calculates heating load
#' 
#' This function calculates heating load.
#' @param utility A utility data frame with columns: energy_type, OAT, usage, end_date and day. Day column: the end date from end_date column.
#' @param cp heating change-point.
#' @param ycp y-values at \code{cp}
#' @export
calc_heat_load <- function(utility, cp, ycp)
{ 
  options(digits=15)
  df = subset(utility, utility$OAT < cp)
  df$usage = (df$usage - ycp)*df$day
  df$usage[df$usage < 0] <- 0 
  heat_load  = sum(df$usage)
  return(heat_load)
}

#' Calculates cooling load
#' 
#' This function calculates cooling load.
#' @param utility A utility data frame with columns, which must have these columns: energy_type, OAT, usage, end_date and day. Day column: the end date from end_date column.
#' @param cp cooling change-point.
#' @param ycp y-values at \code{cp}
#' @export
calc_cool_load <- function(utility, cp, ycp)
{
  options(digits=15)
  df = subset(utility, utility$OAT > cp)
  df$usage = (df$usage - ycp)*df$day
  df$usage[df$usage < 0] <- 0 
  cool_load  = sum(df$usage)
  return(cool_load)
}

#' Ranks baseload, heating/cooling change-points and sensitivity.
#'
#' This function calculates numeric and percent rank for baseload, heating/cooling change-points and sensitivity.
#' @param post_df A data frame returned from \code{\link{post_model_prelimanary}}. The data frame should include multiple buildings to have meaningful interpretation of the rank.
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