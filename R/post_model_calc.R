library(lubridate)

#' Calculates preicted usage
#'
#' This function calcuates predicted usage using parameters from \code{model_params}.
#' @param util Utility dataframe of a single building and a single energy type.
#' @param best_df Best Model dataframe returned by \code{\link{batch_run}}, \code{\link{batch_run_energy}} or \code{\link{construct_model_table}}.
#' @param scaled scaled Boolean value. Defaults to TRUE. Determine to multiply usage with number of days in the month.
#' @export
#' @import lubridate
calc_predicted_usage <- function(util, best_df, scaled = TRUE){
    if(scaled & !is.POSIXlt(util$end_date) & !is.POSIXt(util$end_date) & !is.POSIXct(util$end_date))
    { 
        if (grepl('/', util$end_date[1]))
        {
        util$end_date = strptime(util$end_date, format = "%m/%d/%y")
        }else
        {
        util$end_date = strptime(util$end_date, format = "%Y-%m-%d")
        }
    }

    temp = c(best_df$ycp, best_df$ls, best_df$rs)
    B = as.matrix(c(temp[1],temp[2:3][temp[2:3] != 0 ]))
    util$predicted_usage = y_gen(as.character(.subset2(best_df, 'model_type')[1]),
                          util$OAT, B, .subset2(best_df, 'xcp1')[1],
                          cp2 = .subset2(best_df, 'xcp2')[1])

    if (scaled){
        util$day = as.numeric(format(util$end_date, format = "%d"))
        util$scaled_usage = util$usage * util$day
        util$scaled_predicted_usage = util$predicted_usage * util$day
    }
    
    return(util)
}


calc_not_none <- function(util, ycp){
    return(util['scaled_predicted_usage'] - (util['day']*ycp) )
}


calc_none <- function(util, ycp){
    return(util['predicted_usage']-ycp)
}


calc_true_load <- function(total_predicted_load, total_predicted_consumption, total_consumption){
    return(total_consumption*total_predicted_load/total_predicted_consumption)
}

zero_out_negative_scaled_usage <- function(utility){
    utility$scaled_predicted_usage[utility$scaled_predicted_usage < 0] <- 0
    return(utility)
}

check_ycp <- function(util, model_params){
    model_type = model_params$model_type
    ycp = model_params$ycp

    if (model_type == '2P'){
        return(min(util$usage))
    }

    if(ycp<0){
        xcp1 = model_params['xcp1']
        xcp2 = model_params['xcp2']

        ycp = switch(as.character(model_type),
                '3PC' = stats::median(sort(util$usage[util$OAT < xcp1])),
                '3PH' = stats::median(sort(util$usage[util$OAT > xcp1])),
                '4P' = mean(sort(util$usage[util$OAT])[1:3]),
                '5P' = stats::median(sort(util$usage[util$OAT >= xcp1 & util$OAT <= xcp2]))
        )
    }

    return(ycp)
}

cool_load_2P <- function(util, model_params, total_consumption, total_predicted_consumption, scaled = TRUE){
    ycp = check_ycp(util, model_params)
    rs = model_params$rs

    if (rs < 0){
        return(NA)
    }
    total = 0
    calc_total = ifelse(scaled, calc_not_none, calc_none)
    total = calc_total(util, ycp)
    return(calc_true_load(total, total_predicted_consumption, total_consumption))
} 

heat_load_2P <- function(util, model_params, total_consumption, total_predicted_consumption, scaled = TRUE){
    ycp = check_ycp(util, model_params)

    rs  = model_params$rs

    if(rs >0){
        return(NA)
    }
    calc_total = ifelse(scaled, calc_not_none, calc_none)
    total = calc_total(util, ycp)
    return(calc_true_load(total, total_predicted_consumption, total_consumption))
}


calc_heat_load <- function(util, model_params, total_consumption, total_predicted_consumption, scaled = TRUE){
    ycp = check_ycp(util, model_params)
    xcp1 = model_params$xcp1

    if (model_params$ls >= 0){
        return(NA)
    }

    calc_total = ifelse(scaled, calc_not_none, calc_none)
    temp_util = subset(util, util$OAT < xcp1)
    calc_vec = calc_total(temp_util, ycp)
    calc_vec[calc_vec < 0] <- 0
    total = sum(calc_vec)

    return(calc_true_load(total, total_predicted_consumption, total_consumption))
}

calc_cool_load <- function(util, model_params, total_consumption, total_predicted_consumption, scaled = TRUE){
    ycp = check_ycp(util,model_params)

    xcp1 = model_params$xcp1

    if (model_params$rs <= 0){
         return(NA)
    }

    calc_total = ifelse(scaled, calc_not_none, calc_none)
    temp_util = subset(util, util$OAT > xcp1)
    calc_vec = calc_total(temp_util, ycp)
    calc_vec[calc_vec < 0] <- 0
    total = sum(calc_vec)

    return(calc_true_load(total, total_predicted_consumption, total_consumption))
}


cool_load_5P <- function(util, model_params, total_consumption, total_predicted_consumption, scaled = TRUE){
    ycp = check_ycp(util,model_params)
    xcp2 = model_params$xcp2

    total = 0
    calc_total = ifelse(scaled, calc_not_none, calc_none)
    temp_util = subset(util, util$OAT > xcp2)
    calc_vec = calc_total(temp_util, ycp)
    calc_vec[calc_vec < 0] <- 0
    total = sum(calc_vec)
    return(calc_true_load(total, total_predicted_consumption, total_consumption))
}

#' Calculates total consumption
#'
#' This function calculates total consumption and total predicted consumption.
#' @param utility Utility dataframe returned by \code{\link{calc_predicted_usage}}.
#' @param scaled Boolean value. Defaults to TRUE. Determine to multiply usage with number of days in the month.
#' @export
calc_total_consumption <- function(utility, scaled = TRUE){
    if(scaled){
        total_predicted_consumption = sum(utility$scaled_predicted_usage)
        total_consumption = sum(utility$scaled_usage)
    }else{
        total_predicted_consumption = sum(utility$predicted_usage)
        total_consumption = sum(utility$usage)
    }
    return(list(total_predicted_consumption = total_predicted_consumption, total_consumption = total_consumption))
}

#' Consturct post model table
#'
#' This function makes post model table for a single building and a single energy type. For multiple buildings with multiple energy types, use \code{\link{main_post_model}}.
#' @param utility Utility dataframe. See \code{\link{unretrofit_utility}}.
#' @param model_params Best Model dataframe returned by \code{\link{batch_run}}, \code{\link{batch_run_energy}} or \code{\link{construct_model_table}}.
#' @param scaled Boolean value. Defaults to TRUE. Determine to multiply usage with number of days in the month.
#' @export
make_post_model_table <- function(utility, model_params, scaled = TRUE){
    
    utility = calc_predicted_usage(utility, model_params, scaled)

    if (scaled){
        utility = zero_out_negative_scaled_usage(utility)
    }
    
    total_ls = calc_total_consumption(utility, scaled)
    total_predicted_consumption = total_ls$total_predicted_consumption
    total_consumption = total_ls$total_consumption

    loads_list = calc_loads(utility, model_params, total_consumption, total_predicted_consumption, scaled)
    post_model = data.frame(bdbid = model_params$bdbid, prepost = model_params$prepost,
                        total_consumption = total_consumption, baseload = loads_list$baseload,
                        cool_load = loads_list$cool_load, heat_load = loads_list$heat_load,
                        model_type = model_params$model_type, energy_type = model_params$energy_type,
                        period = nrow(utility)
                )
    named_param_df = heat_cool_sen_change(model_params)
    post_model = cbind(post_model, named_param_df)
    return(post_model)
}

#' Calculates loads
#'
#' This function calculates heating load, cooling load and baseload.
#' @param util Utility data frame of a single building and single energy type, returned by \code{\link{calc_predicted_usage}}.
#' @param model_params List or dataframe returned by \code{\link{construct_model_table}}.
#' @param total_predicted_consumption Total predicted consumption (i.e sum of usages calculated using parameters from \code{model_params}).
#' @param total_consumption Total consumption.
#' @param scaled Boolean. Defaults to TRUE. Determine to multiply usage with number of days in the month.
#' @export
calc_loads <- function(util, model_params, total_predicted_consumption, total_consumption, scaled = TRUE){
    model = model_params$model_type

    cooling_load = switch(as.character(model),
                    '2P' = cool_load_2P(util, model_params, total_consumption, total_predicted_consumption, scaled),
                    '3PC' = calc_cool_load(util, model_params, total_consumption, total_predicted_consumption, scaled),
                    '3PH' = calc_cool_load(util, model_params, total_consumption, total_predicted_consumption, scaled),
                    '4P' = calc_cool_load(util, model_params, total_consumption, total_predicted_consumption, scaled),
                    '5P' = cool_load_5P(util, model_params, total_consumption, total_predicted_consumption, scaled)
                )
    
    heating_load = switch(as.character(model),
                    '2P' = heat_load_2P(util, model_params, total_consumption, total_predicted_consumption, scaled),
                    '3PC' = calc_heat_load(util, model_params, total_consumption, total_predicted_consumption, scaled),
                    '3PH' = calc_heat_load(util, model_params, total_consumption, total_predicted_consumption, scaled),
                    '4P' = calc_heat_load(util, model_params, total_consumption, total_predicted_consumption, scaled),
                    '5P' = calc_heat_load(util, model_params, total_consumption, total_predicted_consumption, scaled)
                )
    baseload = calculate_baseload(total_consumption, heating_load, cooling_load)
    return(list(baseload = baseload, cool_load = cooling_load, heat_load = heating_load))
}

heat_cool_sen_change <- function(model_params)
{ 
  options(digits=15)
  model = as.character(model_params$model_type)
  df = data.frame(heating_change_point  = switch(model,
                "3PC" = .subset2(model_params, 'xcp1')[1], "3PH" = .subset2(model_params, 'xcp1')[1],
                "4P" = .subset2(model_params, 'xcp1')[1],
                "5P" = .subset2(model_params, 'xcp1')[1], NA)) #set zero or NULL later for elec

  df$cooling_change_point = switch(model,
                "3PC" = .subset2(model_params, 'xcp1')[1], "3PH" = .subset2(model_params, 'xcp1')[1],
                "4P" = .subset2(model_params, 'xcp1')[1],
                "5P" = .subset2(model_params, 'xcp2')[1], NA)

  df$heating_sensitivity  = switch(model,
                "3PH" = .subset2(model_params, 'ls')[1],
                "4P" = .subset2(model_params, 'ls')[1],
                "5P" = .subset2(model_params, 'ls')[1], "3PC" = 0,
                "2P" = ifelse(.subset2(model_params, 'ls')[1] < 0, .subset2(model_params, 'ls')[1], 0))


  df$cooling_sensitivity = switch(model,
                "3PC" = .subset2(model_params, 'rs')[1],
                "4P" = .subset2(model_params, 'rs')[1],
                "5P" = .subset2(model_params, 'rs')[1], "3PH" = 0,
                "2P" = ifelse(.subset2(model_params, 'ls')[1] > 0, .subset2(model_params, 'ls')[1], 0))

  return(df)
}

#' main_post_model
#'
#' Main function for generating post model output;  transforms best_model in a more meaningful way and calculates loads. Also can be used for retrofit data.
#' @param utility Utility dataframe. See \code{\link{unretrofit_utility}} for the format.
#' @param best_model Best Model dataframe returned by \code{\link{batch_run}}, \code{\link{batch_run_energy}} or \code{\link{construct_model_table}}.
#' @param scaled Boolean value. Defaults to TRUE. Determine to multiply usage with number of days in the month.
#' @export
#' @examples
#' \dontrun{
#' batch_result = batch_run(unretrofit_utility)
#' post_df = main_post_model(unretrofit_utility, batch_result$best_result_df)
#' }
main_post_model <- function(utility, best_model, scaled = TRUE)
{ 
  options(digits=15)
  post_df = data.frame()

  if(scaled & !('end_date' %in% colnames(utility))){
      stop('scaled is set to TRUE but end_date column is missing in utility.')
  }

  if(is.factor(.subset2(best_model, 'model_type')[1])){
    best_model$energy_type = as.character(best_model$energy_type)
    best_model$model_type = as.character(best_model$model_type)
  }

  for (i in 1:nrow(best_model)){ 
    model_params = best_model[i,]
    util = subset(utility, utility$bdbid == model_params$bdbid &
                            utility$energy_type == model_params$energy_type &
                            utility$prepost == model_params$prepost)
    post_model = make_post_model_table(util, model_params, scaled)
    post_df = rbind(post_df, post_model)
  }
  return(post_df)
}

calculate_baseload <- function(total_consumption, heating_load, cooling_load){
    if (is.na(heating_load) & is.na(cooling_load)){
        return(NA)
    }

    if (is.na(heating_load)){
        return(total_consumption - cooling_load)
    }else if(is.na(cooling_load)){
        return(total_consumption - heating_load)
    }else{
        return(total_consumption - heating_load - cooling_load)
    }
}

#' Ranks baseload, heating/cooling change-points and sensitivity.
#'
#' This function calculates numeric and percent rank for baseload, heating/cooling change-points and sensitivity (for a sinlge energy type).
#' @details
#' If \code{post_df} includes more than one energy type, it is advised to use \code{\link{main_post_model}}.
#' @param post_df A data frame returned from \code{\link{main_post_model}}. The data frame should include multiple buildings to have meaningful interpretation of the rank.
#' @export
#' @examples
#' \dontrun{
#' batch_result = batch_run(unretrofit_utility)
#' post_df = main_post_model(unretrofit_utility, batch_result$best_result_df)
#' post_elec = subset(post_df, post_df$energy_type == 'Elec')
#' lean_elec = lean_analysis_ranking(post_elec)
#' }
lean_analysis_ranking <- function(post_df)
{ 
  lean_df = data.frame(bdbid = post_df$bdbid, model_type = post_df$model_type,
                        prepost = post_df$prepost, energy_type = post_df$energy_type)

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

#' Main logic for lean analysis
#'
#' Main logic for lean analysis. Can handle multiple energy types.
#' @param post_df A data frame returned from \code{\link{main_post_model}}. The data frame should include multiple buildings to have meaningful interpretation of the rank.
#' @export
#' @examples
#' \dontrun{
#' batch_result = batch_run(unretrofit_utility)
#' post_df = main_post_model(unretrofit_utility, batch_result$best_result_df)
#' lean_df = main_lean_analysis(post_df)
#' }
main_lean_analysis <- function(post_df){
    lean_df = data.frame()
    for (energy in unique(post_df$energy_type)){
        post_energy = subset(post_df, post_df$energy_type == energy)
        temp = lean_analysis_ranking(post_df)
        lean_df = rbind(lean_df, temp)
    }
    return(lean_df)
}