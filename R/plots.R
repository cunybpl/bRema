#' Handles plotting parameter models
#' 
#' This function handles the general scheme of plotting parameters model by using plotly. Uses \code{main_line_point_plot} and \code{plot_point} functions to handle and draw graphs.
#' @param util A data frame with columns: OAT, usage, estimated (1 for estimated usage and 0 for actual usage) and prepost (1 for unretrofit and pre-retrofit and 3 for post-retrofit). See \code{\link{unretrofit_utility}} for more information about data format.
#' @param best_model A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @param b_name Plot title (Building Name or Buidling ID). Defaults to an empty string.
#' @param unretrofit_flag A boolean value to indicate whether it is retrofit or unretrofit. Defaults to \code{TRUE}. If it is retrofit, set this to \code{FALSE}. This only affects parameter model graph's legend labels.
#' @import plotly
#' @export
#' @seealso \code{\link{plot_point}} and \code{\link{main_line_point_plot}}
main_plot_handler <- function(util, best_model, energy, b_name = '', unretrofit_flag = TRUE)
{ 
  #require(plotly)
  x1 = util$OAT
  y1 = util$usage
  temp_est = util$estimated

  pre_key = unique(util$prepost)

  if (unretrofit_flag)
  {
    pre_key = 0
  }

  z1 = c(1:length(temp_est))
  z1[temp_est == 1] <- 'Est'
  z1[temp_est != 1] <- 'Act'

  df1 = data.frame(x = x1, y = y1, z = z1)

  if (is.null(best_model) | length(best_model) == 0)
  {
    final_figure = plot_point(df = df1, energy = energy, pre_key = pre_key, model_fig = plot_ly(), b_name = b_name)
  }else
  {
    final_figure = main_line_point_plot(df1, best_model, energy, pre_key, b_name)
  }
  return(final_figure)
}


#' Handles plotting parameter models
#' 
#' This function returns parameters model plot. Uses \code{plot_model} and \code{plot_point} to draw parameter models.
#' @param df A data frame with columns: x, y, z (OAT, usage, estimated from utillity data frame). Column 'z' must be a character column with 'Est' for estimated (or 1) and 'Act' for acutal (or 0).
#' @param best_model A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @param pre_key A numeric value. 0 for unretrofit, 1 for pre-retrofit and 3 for post-retrofit. Defaults to 0.
#' @param b_name Plot title (Building Name or Buidling ID).
#' @export
#' @seealso \code{\link{plot_point}} and \code{\link{main_plot_handler}}
main_line_point_plot <- function(df, best_model, energy, pre_key = 0, b_name)
{   
	#require(plotly)
    B = best_model$params #not params, just ycp, and slope(s)
    x1 = df$x

    model_fig = plot_model(x = x1, model = best_model$model, B = B,
        					cp1 = best_model$cp1, cp2 = best_model$cp2,
        					energy = energy, pre_key = pre_key, unit = FALSE, p1 = plot_ly())
    final_figure = plot_point(df = df, energy = energy, pre_key = pre_key,
                              model_fig = model_fig, b_name = b_name)
    return(final_figure)
}

#' Handles plotting points for parameter models
#' 
#' This function returns a scatter plot.
#' @param df A data frame with columns: x, y, z (OAT, usage, estimated from utillity data frame)
#' @param energy A character string. Energy Type, 'Elec' or 'Fuel'.
#' @param pre_key A numeric value. 0 for unretrofit, 1 for pre-retrofit and 3 for post-retrofit. Defaults to 0.
#' @param model_fig A plotly object. Defaults to \code{plot_ly()}.
#' @param b_name Plot title (Building Name or Buidling ID). Defaults to an empty string.
#' @export
#' @seealso \code{\link{plot_model}}
plot_point <- function(df, energy, pre_key = 0, model_fig = plot_ly(), b_name = '')
{ 
  #require(plotly)
  util_act = subset(df, df$z == 'Act')
  util_est = subset(df, df$z == 'Est')


  if(pre_key == 0){
	name_n = energy
  }else if (pre_key == 1)
  {
	name_n = paste('Pre', energy)
  }else
  {
	name_n = paste('Post', energy)
  }

  switch(as.character(energy),
        'Elec' = 
        {
          if(pre_key == 1 |  pre_key == 0)
          {color_n = 'rgba(51, 113, 213, 1)'}else{color_n = 'rgba(109, 203, 15, 1)'}
          y_title = "Usage (kWh)"
        },
        'Fuel' = 
        {
          if(pre_key == 1 |  pre_key == 0)
          {color_n = 'rgba(240, 24, 28,1)'}else{color_n = 'rgba(109, 203, 15, 1)'}
          y_title = "Usage (BTU)"
        }
    )

    #point_fig = geom_point(data = df, aes(x = x, y= y, shape= factor(z), color = 'Elec Consumption'))
    point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y,
    						type ='scatter', mode ='markers',
    						marker = list(symbol = 'circle', color = color_n, size = 9),
    						name = paste(name_n, 'Act', sep = ' '), inherit = FALSE)
 
    point_fig = add_trace(p = point_fig_act, x = ~util_est$x, y = ~util_est$y,
    					type ='scatter', mode ='markers',
    					marker = list(symbol = 'circle-open', color = color_n, size = 9),
    					name = paste(name_n, 'Est', sep = ' '), inherit = FALSE) %>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100),
      xaxis = list(title = "Temperature"),
      yaxis = list(title = y_title))
  return(point_fig)
}

#' Handles plotting lines for parameter models
#' 
#' This function returns a line plot.
#' @param x A vector (OAT).
#' @param model A character string. Model: '2P', '3PC', '3PH', '4P', or '5P'.
#' @param B A matrix, either 2 by 1 or 3 by 1 matrix. B[1] = y-value at cp1 (and cp2), B[2] = slope (if there are two slopes, this is the leftmost slope), B[3] = slope (if there are two slopes, this is the rightmost slope)
#' @param cp1 A numeric value. If there are two change-points, this is the leftmost change-point.
#' @param cp2 A numeric value. If there are two change-points, this is the rightmost change-point. If there is only one change-point, set this to be zero. 
#' @param energy A character string. Energy type, either 'Elec' or 'Fuel'.
#' @param pre_key A numeric value. 0 for unretrofit, 1 for pre-retrofit and 3 for post-retrofit. Defaults to 0.
#' @param unit A boolean value. Determines whether or not to convert kWh to BTU.
#' @param p1 A plotly object. Defaults to \code{plot_ly()}.
#' @export
#' @seealso \code{\link{plot_point}}
plot_model <- function(x, model, B, cp1, cp2, energy, pre_key = 0, unit, p1 = plot_ly())
{ 
  options(digits=15)
  #require(plotly)
  x = x[order(x)]
  x0 = x[1]
  xf = x[length(x)]
  Ycp= B[1] #coeff, need this to get y intercept
  slope1 = B[2]
  yInter_1 = Ycp - slope1*cp1

  if (length(B) == 3) # 4P or 5p
  {
    slope2 = B[3]
    if (cp2 == 0) # 4P
    {
      yInter_2 = Ycp - slope2*cp1
    }else # 5P
    {
      yInter_2 = Ycp - slope2*cp2
    }
  }


  estimated = switch(model,
  	"5P" = {
  		x = c(x0, cp1, cp2, xf)
  		estimated = c(1,2,3,4)
   		estimated[x <= cp1] <- yInter_1 + slope1*x[x <= cp1]
   		estimated[x >= cp2] <- yInter_2 + slope2*x[x >= cp2]
   		estimated[ x > cp1 & x < cp2] <- Ycp
   		estimated
  	},
  	"4P" = {
		x = c(x0, cp1, xf)
		estimated = c(1,2,3)
		estimated[x <= cp1] <- yInter_1 + slope1*x[x <= cp1] 
		estimated[x > cp1] <- yInter_2 + slope2*x[x > cp1]
		estimated
  		},
  	"3PH" = {
  		x = c(x0, cp1, xf)
  		estimated = c(1,2,3)
  		estimated[x <= cp1] <- yInter_1 + slope1*x[x <= cp1]
  		estimated[x > cp1] <- Ycp
  		estimated
  	},
  	"3PC" = {
  		x = c(x0, cp1, xf)
  		estimated = c(1,2,3)
    	estimated[x > cp1] <- yInter_1 + slope1*x[x > cp1]
    	estimated[x <= cp1] <- Ycp
    	estimated
  	},
  	"2P" = {
  		x = c(x0, xf)
  		estimated = yInter_1 + slope1*x 
  		estimated
  	}
  	)

  if (unit){
    df = data.frame(x = x, y = estimated*3412.14)
  }else
  {
    df = data.frame(x = x, y = estimated)
  }

  if(pre_key == 0){
	name_n = paste(energy, 'Model')
  }else if (pre_key == 1)
  {
	name_n = paste('Pre', energy, 'Model')
  }else
  {
	name_n = paste('Post', energy, 'Model')
  }

  switch(as.character(energy), 
          'Elec' = 
          {
            if(pre_key == 1 |  pre_key == 0)
            {color_n = 'rgba(51, 113, 213, 1)'}else{color_n = 'rgba(109, 203, 15, 1)'}
          },
          'Fuel' = 
          {
            if(pre_key == 1 |  pre_key == 0){color_n = 'rgba(240, 24, 28,1)'}
            else{color_n = 'rgba(109, 203, 15, 1)'}
          }
    )

  model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter',
      						mode = 'lines', line = list(color = color_n),
      						name = name_n, inherit = FALSE)
  return(model_fig)
}

#' Plots time series
#' 
#' This function plots time series graph for date against usage and OAT. This function can be used for both retrofit and unretrofit data.
#' @param util A data frame with columns: OAT, usage, estimated (1 for estimated usage and 0 for actual usage). See \code{\link{unretrofit_utility}} and \code{\link{retrofit_utility}}for more information about data format.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @export
#' @seealso \code{\link{plot_point}} and \code{\link{main_line_point_plot}}
#' @examples
#' \dontrun{util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec')
#' plot_timeseries(util, 'Elec')}
plot_timeseries <- function(util, energy)
{ 
  #require(plotly)
  options(digits=15)

  util$estimated[util$estimated == 1] <- 'Est'
  util$estimated[util$estimated != 1] <- 'Act'


  util_act = subset(util, util$estimated == 'Act')
  util_est = subset(util, util$estimated == 'Est')


  switch(as.character(energy), 
          'Elec' = 
          {
            title_n = 'Usage(kWh/sq/month)'
            usage_color = 'rgba(51, 113, 213, 1)'
          },
          'Fuel' = 
          {
            title_n = 'Usage(BTU/sqft/month)'
            usage_color = 'rgba(240, 24, 28,1)'
          }
          )

  ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "OAT"
  )

    p <- plot_ly() %>%
      add_trace(x = ~util$end_date, y = ~util$OAT, type ='scatter',
      			mode = 'lines', line = list(color = 'rgba(243, 154, 36, 1)'),
      			name = "OAT", yaxis = "y2") %>%
      add_trace(x = ~util$end_date, y = ~util$usage, type ='scatter',
      			mode = 'lines', line = list(color = usage_color),
      			name = "Usage") %>%
      add_trace(x = ~util_est$end_date, y = ~util_est$usage, type ='scatter', mode ='markers',
      			marker = list(symbol = 'circle-open', color = usage_color, size = 9),
      			name = 'Est') %>%
      add_trace(x = ~util_act$end_date, y = ~util_act$usage, type ='scatter', mode ='markers',
      			marker = list(symbol = 'circle', color = usage_color, size = 9),
      			name = 'Act') %>%
      layout(
      title = "Time Series", yaxis2 = ay, yaxis = list(title = title_n),
      				margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date)
    )


  return(p)
}