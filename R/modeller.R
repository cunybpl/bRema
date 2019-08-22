#' Linear regression
#' 
#' This function returns parameters of least square equation given column matrices of independent variables and dependent matrices.
#' @param xmat Independent matrix (dimesions, either n by 2 or n by 3), where n is number of sample points. Matrix returned by \code{make_matrix} function.
#' @param ymat Dependent matrix (dimesions, n by 1), where n is number of sample points.
#' @export
#' @return A matrix with components:
#' \item{ycp}{y-value at the change-point}
#' \item{Slope1}{leftmost slope if B is 3 by 1 matrix. Otherwise, non-zero slope.}
#' \item{Slope2}{rightmost slope if if B is 3 by 1 matrix.}
#' @seealso \code{\link{make_matrix}}
#' @examples
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec')
#' temp = sort_matrix(util$OAT,util$usage)
#' x_split = splitter('4P', temp$x, 51)
#' xmat = make_matrix(x_split)
#' B = least_squares(xmat, temp$y)
least_squares <- function (xmat, ymat) # B = [b1 b2 b3], b1 = coeff, b2 = slope 1, b3 = slope 2
{	
	options(digits=10)
	A = t(xmat) %*% xmat
	G = t(xmat) %*% ymat

	Ainv = tryCatch(solve(A), error = function(e){0})
	if (is.matrix(Ainv))
	{
		B = Ainv %*% G
	}else
	{
		B = matrix(0, nrow = 2) #or just set this to zero???
	}
	row.names(B) = if (nrow(B)==2) c('ycp', 'Slope1') else c('ycp', 'Slope1', 'Slope2')
	return(B)
}

#' Splits and restructures a matrix
#' 
#' @description
#' This function produces a matrix based on different change-points inside an interval and models.
#' For example, if the model is '3PC', then any points in \code{xmat} less than the change-point,
#' \code{cp1}, will be set to be zero and \code{cp1} will be substracted from the rest of the points.
#'
#' @param model A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.
#' @param xmat Independent Column matrix (dimesion, n by 1).
#' @param cp1 A numeric value. The first change-point. Defaults to NA.
#' @param cp2 A numeric value. The second change-point. Defaults to NA.
#' @export
#' @examples
#' x = matrix(c(1,2.3,2.78, 3.4,5.5, 6.1, 7 ,8.4, 9,10))
#' x_split = splitter('4P', x, 3)
splitter <- function (model, xmat, cp1  = NA, cp2 = NA)
{	
	options(digits=15)

	cfinal = switch(model, 
		"2P" = xmat,
		"3PC" = {
				xmat[xmat <= cp1] <- 0.0
				xmat[xmat >= cp1 ] <- xmat[xmat >= cp1 ] - cp1
				xmat
			},
		"3PH" = {
				xmat[xmat <= cp1] <- xmat[xmat <= cp1] - cp1
				xmat[xmat > cp1 ] <- 0.0
				xmat
		},
		"4P" = {
				temp = xmat

				temp[temp <= cp1] <- temp[temp <= cp1] - cp1
				temp[temp > cp1] <- 0.0
				c1 = temp

				temp = xmat

				temp[xmat <= cp1] <- 0.0
				temp[xmat > cp1] <- temp[xmat > cp1] - cp1
				c2 = temp

				cbind(c1, c2)
		},
		"5P" = {
				temp = xmat
				temp[temp <= cp1] <- temp[temp <= cp1] - cp1
				temp[temp > cp1] <- 0.0
				c1 = temp

				temp = xmat
				temp[xmat <= cp2] <- 0.0
				temp[xmat > cp2] <- temp[xmat > cp2] - cp2
				c2 = temp

				cbind(c1, c2)
		}
		)
	return(cfinal)
}

#' Restructures a matrix returned by \code{splitter} function
#' 
#' This function adds a column of zeros and ones to the matrix returned by \code{splitter} function.
#' The output is used in \code{least_squares} function as independent matrix input.
#' @param xsplit A column matrix (manipulated by \code{splitter} function based on change-point(s) and model).
#' @export
#' @seealso \code{\link{splitter}}
#' @examples
#' x = matrix(c(1,2.3,2.78, 3.4,5.5, 6.1, 7 ,8.4, 9,10))
#' x_split = splitter('4P', x, 3)
#' x_final = make_matrix(x_split)
make_matrix <- function (xsplit) 
{	
	options(digits=15)
	if (ncol(xsplit) == 1)
	{
		c1 = xsplit
		c2 = 0
	}else
	{
		c1 = matrix(xsplit[,1], nrow = nrow(xsplit))
		c2 = matrix(xsplit[,2], nrow = nrow(xsplit))
	}
	if (is.matrix(c2))
	{	
		temp = matrix(1:nrow(xsplit))
		temp[c1 !=0 | c2 != 0] <- 1
		temp[c1 ==0 & c2 == 0] <- 0
		return(cbind(temp,c1,c2))
	}else
	{	
		temp = matrix(1:nrow(xsplit))
		temp[xsplit != 0] <-  1
		temp[xsplit == 0] <- 0
		return(cbind(temp,c1))
	}
}

#' Checks the length of a matrix
#' 
#' This function checks if less than 25 percent of the entries of a matrix is zero or not. It returns \code{FALSE} only if less than 25 percent of matrix entries is non-zero.
#' @param xsplit A column matrix (manipulated by \code{splitter} function based on change-point(s) and model).
#' @export
#' @examples
#' x = matrix(c(1,2.3,2.78, 3.4,5.5, 6.1, 7 ,8.4, 9,10))
#' x_split = splitter('4P', x, 3)
#' matrix_length(x_split)
matrix_length <- function(xsplit)
{	
	options(digits=15)
	if (ncol(xsplit) == 1)
	{
		c1 = xsplit
		c2 = 0 
	}else
	{
		c1 = matrix(xsplit[,1], nrow = nrow(xsplit))
		c2 = matrix(xsplit[,2], nrow = nrow(xsplit))
	}
	len_c1 = length(c1)/4.0
	flag = TRUE
	if (len_c1 > length(c1[c1!=0]))
	{
		flag = FALSE
	}
	if (is.matrix(c2))
	{
		if (len_c1 > length(c2[c2!=0])){
			flag = FALSE
		}
	}
	return(flag)
}

#' Calculates statistics
#' 
#' This function returns a matrix of 'RMSE', 'Rsquared', 'CV_RMSE', 'SSE', 'SST', 'MSE' given actual and predicted values.
#' @param y A (column) matrix of actual values(usage).
#' @param y_predict A (column) matrix of predicted values(usage).
#' @export
#' @examples
#' y_predict = matrix(c(1:10))
#' y = matrix(c(1,2.5,3,4,5,5.5,7,8.3,9,10))
#' stat_matrix = stats(y, y_predict)
stats <- function(y, y_predict)
{	
	options(digits=15)
	SSE = sum((y - y_predict)^2)
	SST = sum((y - mean(y))^2)
	MSE = SSE/length(y)
	Rsquared = 1 - (SSE/SST)
	RMSE = sqrt(MSE)
	CV_RMSE = (RMSE/mean(y)) 
	stats_matrix= matrix(c(RMSE, Rsquared, CV_RMSE, SSE, SST, MSE), nrow = 6)
	row.names(stats_matrix) = c('RMSE', 'Rsquared', 'CV_RMSE', 'SSE', 'SST', 'MSE')
	return(stats_matrix) #compare RMSE
}

#' Calculates predicted usage
#' 
#' This function returns a matrix of estimated usage given a specific model, independent variable matrix, parameter matrix and change-point(s).
#' @param model A charater string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.
#' @param x A vector. Independent variable.
#' @param B A matrix, either 2 by 1 or 3 by 1 matrix. \code{B[1]} = y-value at cp1 (and cp2), \code{B[2]} = slope (if there are two slopes, this is the leftmost slope), \code{B[3]} = slope (if there are two slopes, this is the rightmost slope).
#' @param cp1 A numeric value. The first change-point. Defaults to NA.
#' @param cp2 A numeric value. The second change-point. Defaults to NA.
#' @export
#' @examples
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec')
#' temp = sort_matrix(util$OAT,util$usage)
#' x_split = splitter('4P', temp$x, 51)
#' xmat = make_matrix(x_split)
#' B = least_squares(xmat, temp$y)
#' y_est = y_gen('4P', temp$x, B, 51, 0)
y_gen <- function(model, x, B, cp1=NA, cp2 = NA) #input x is unmodified original x
{	
	options(digits=15)
	Ycp= B[1] #coeff, need this to get y intercept
	slope1 = B[2]

	yInter_1 = switch(model, '2P' = Ycp, Ycp - slope1*cp1)

	if (length(B) == 3) # 4P or 5p
	{
		slope2 = B[3]
		if (is.na(cp2)) # 4P
		{
			yInter_2 = Ycp - slope2*cp1
		}else # 5P
		{
			yInter_2 = Ycp - slope2*cp2
		}
	}

	y_predict = matrix(1:length(x))
	switch(model, 
		"5P" = {
			y_predict[x <= cp1 ] <- yInter_1 + slope1*x[x <= cp1]
			y_predict[x >= cp2] <- yInter_2 + slope2*x[x >= cp2]
			y_predict[x > cp1 & x < cp2] <- Ycp
			return(y_predict)
		},
		"4P" = {
			y_predict[x <= cp1] <- yInter_1 + slope1*x[x <= cp1]
			y_predict[x > cp1] <- yInter_2 + slope2*x[x > cp1]
			return(y_predict)
		},
		"3PH" = {
			y_predict[x <= cp1] <-  yInter_1 + slope1*x[x <= cp1]
			y_predict[x > cp1] <- Ycp
			return(y_predict)
		},
		"3PC" = {
			y_predict[x > cp1] <- yInter_1 + slope1*x[x > cp1]
			y_predict[x <= cp1] <- Ycp
			return(y_predict)
		},
		"2P" = {
			y_predict = yInter_1 + slope1*x
			return(y_predict)
		}
		)
}

#' Performs segmented regression for a given model
#' 
#' This function returns a list which contains stats matrix, parameter matrix and change-point(s).
#' @param model A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.
#' @param x A (column) matrix of outside air temperature, independent variables.
#' @param y A (column) matrix of actual values(usage), dependent variables.
#' @param cp1 A numeric value. The first change-point. Defaults to NA.
#' @param cp2 A numeric value. The second change-point. Defaults to NA.
#' @export
#' @examples
#' x = matrix(c(1:10))
#' y = matrix(c(1,2.5,3,4,5,5.5,7,8.3,9,10))
#' result = test_model('4P', x, y, 4)
test_model <- function(model, x,y, cp1=NA, cp2 = NA) #use this with a specific change-point, call sort matrix first.
{	
	options(digits=15)
	xsplit = splitter(model, x, cp1, cp2)
	if (matrix_length(xsplit))
	{
		xfinal = make_matrix(xsplit)
		B = least_squares(xfinal, y)
		y_predict = y_gen(model, x, B, cp1, cp2)
		stats_matrix = stats(y, y_predict)
		test_value = list(stats = stats_matrix, params = B, cp1 = cp1,
							cp2 = cp2, model = model)
	}else
	{
	  test_value = list(stats = matrix(Inf), cp1 = Inf)
	}
	return (test_value)
}

#' Calculates the best segmented regression parameters for a given model
#' 
#' This function returns the best change-point parameter based model given a specific model and independent and dependent column matrices by using only one step size. 
#' This function is dependent on \code{test_model} and \code{splitter} function.
#' @param model A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.
#' @param x A (column) matrix of outside air temperature, independent variables.
#' @param y A (column) matrix of actual values(usage), dependent variables.
#' @param step A single numeric value. Defaults to 0.5.
#' @return A list with components:
#' \item{stats}{A matrix of statistical results such as RMSE, R-squared}
#' \item{params}{A matrix of parameters such as basload and slopes}
#' \item{cp1}{Change point. The leftmost change point. This will be zero if \code{model} is '2P'.}
#' \item{cp2}{Change point. The rightmost change point. This will be zero if \code{model} is not '5P'.}
#' \item{model}{Model type.}
#' @export
#' @seealso \code{\link{create_model_2}}, and \code{\link{run_model}}
#' @examples
#' x = matrix(c(1:10))
#' y = matrix(c(1,2.5,3,4,5,5.5,7,8.3,9,10))
#' result = create_model(x, y, '4P')
create_model <- function (x, y, model, step = 0.5)
{	
	options(digits=15)
	low_temp = x[length(x)/4]
	high_temp = x[length(x) - (length(x)/4)]
	bestvalue = list(stats = matrix(Inf))
	#step = [10,2,0.5]
	if (model == '2P')
	{
		bestvalue = test_model(model, x,y)
	}else
	{
		for (cp1 in seq(low_temp,high_temp, by = step))
		{	
			if (model != '5P')
			{
				test_value = test_model(model, x,y, cp1)
				if (test_value$stats[1,1] < bestvalue$stats[1,1])
				{	
					bestvalue = test_value
				}
			}else #5P starts and "for loops" for not 5P ends here
			{
			  
				for (cp2 in seq(cp1, high_temp, by = step))
				{
					test_value = test_model(model, x,y, cp1, cp2)
					if (test_value$stats[1,1] < bestvalue$stats[1,1])
					{	
						bestvalue = test_value
					}
				}
			} #5P ends here
		}# the first main loop here
	} #not 2P ends here
	return(bestvalue)
}

#' Calculates the best segmented regression parameters for a given model
#' 
#' This function returns the best change-point parameter based model given a specific model and independent and dependent column matrices by using three different step sizes. Note that \code{create_model} function does not use multiple step sizes.
#' This function is dependent on \code{test_model} and \code{splitter} function.
#' @param model A charater string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.
#' @param x A (column) matrix of outside air temperature, independent variables.
#' @param y A (column) matrix of actual values(usage), dependent variables.
#' @param step A numeric vector. Defaults to \code{c(10,2,0.5)}. Adjust this depending on values of your independent variables.
#' @return A list with components:
#' \item{stats}{A matrix of statistical results such as RMSE, R-squared}
#' \item{params}{A matrix of parameters such as basload and slopes}
#' \item{cp1}{Change point. The leftmost change point. This will be zero if \code{model} is '2P'.}
#' \item{cp2}{Change point. The rightmost change point. This will be zero if \code{model} is not '5P'.}
#' \item{model}{Model type.}
#' @export
#' @seealso \code{\link{create_model}}, and \code{\link{run_model}}
#' @examples
#' x = matrix(c(1:10))
#' y = matrix(c(1,2.5,3,4,5,5.5,7,8.3,9,10))
#' result = create_model_2(x, y, '4P', step = c(2,1,0.5))
create_model_2 <- function (x, y, model, step = c(10,2,0.5))
{	
	options(digits=15)
	low_temp = x[length(x)/4]
	high_temp = x[length(x) - (length(x)/4)]
	bestvalue = list(stats = matrix(Inf))
	if (model == '2P')
	{
		bestvalue = test_model(model, x,y)
	}else
	{	for (step_i in step)
		{
			for (cp1 in seq(low_temp,high_temp, by = step_i))
			{	
				if (model != '5P')
				{
					test_value = test_model(model, x,y, cp1)
					if (test_value$stats[1,1] < bestvalue$stats[1,1])
					{	
						bestvalue = test_value
					}
				}else #5P starts and "for loops" for not 5P ends here
				{
				  #print('cp1')
				  #print(cp1)
					for (cp2 in seq(cp1, high_temp, by = step_i))
					{
					  #print('cp2')
					  #print(cp2)
						test_value = test_model(model, x,y, cp1, cp2)
						if (test_value$stats[1,1] < bestvalue$stats[1,1])
						{	
							bestvalue = test_value
						}
					}
				} #5P ends here
			}# the first main loop here
			new_low = bestvalue$cp1 - step_i
			new_high = bestvalue$cp1 + step_i
			if (new_low > low_temp){low_temp = new_low}
			if (new_high < high_temp){high_temp = new_high}
		}
	} #not 2P ends here
	return(bestvalue)
}

#' Sorts matrices
#' 
#' This function sorts vector \code{x} in increasing order and sorts vector \code{y} in the order of sorted \code{x}.
#' The output is a list with two matrices, \code{x} and \code{y}.
#' @param x A vector of independent variables.
#' @param y A vector of dependent variables. Defaults to zero matrix.
#' @export
#' @examples
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec')
#' temp = sort_matrix(util$OAT,util$usage)
#' xmat = temp$x
#' ymat = temp$y
sort_matrix <- function(x,y=matrix(0))
{
	y = matrix(y[sort.list(x)], nrow = length(y))
	x = matrix(x[sort.list(x)], nrow = length(x))
	return(list(x=x, y=y))
}

ftest <- function(SSE_L, SSE_M, n) #RSS residual sum squares == SSE sum squares of errors (resuiduals) not the same as SSR which is sum squares of regression
{
	fvalue = 0.5*(SSE_L-SSE_M)/(SSE_M/(n-3))
	return(fvalue)
}

#' Calculates and graphs parameter based models
#' 
#' @description
#' This function returns parameters, stats, change-point(s), and parameter model graphs using \code{create_model} function.
#'
#' @details
#' This function can be used for both retrofit and unretrofit data.
#' However, if you want to run this function with retrofit data, \code{subset} or \code{filter} the data first by prepost.
#' If input \code{util} data frame is not in the exact format as \code{\link{unretrofit_utility}}, change independent varaible column to 'OAT' and dependent variable to 'usage', add 'energy_type' column and set all the values to either 'Fuel' or 'Elec'. If 'estimated' is column is not present, a column of ones will be added automatically.
#' @param util A data frame with columns columns: energy_type, OAT, usage and estimated (1 for estimated usage and 0 for actual usage). See \code{\link{unretrofit_utility}} for more information about data format.
#' @param plot_flag A boolean value. Defaults to \code{FALSE}. If set to \code{FALSE}, it will not return any plots.
#' @param step A single numeric value or a vector. If it is a single numeric value, \code{create_model} will be used to find change-point(s). If it is a vector with more than one component, \code{create_model_2} will be used. Defaults to 0.5. Adjust \code{step}-size depending on independent varaiables.
#' @param n A numeric value that determines threshold for population test: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.
#' @param unretrofit_flag A boolean value to indicate whether it is retrofit or unretrofit. Defaults to \code{TRUE}. This only affects parameter model graph's legend labels.
#' @export
#' @seealso \code{\link{create_model}}, \code{\link{create_model_2}}, \code{\link{run_model_retrofit}}, \code{\link{unretrofit_utility}} and \code{\link{retrofit_utility}}
#' @section Note:
#' If more than half of usage points (dependent variables) of input data frame are zeros, error message will be generated.
#' @examples
#' \dontrun{
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec') #unretrofit data
#' result = run_model(util) #result for unretrofit data
#' retrofit_util = subset(retrofit_utility, retrofit_utility$bdbid == 846152
#'						& retrofit_utility$energy_type == 'Elec') #retrofit data
#' pre_util = subset(retrofit_util, retrofit_util$prepost == 1)#subset pre-retrofit utility data
#' pre_result = run_model(pre_util) #result for pre-retrofit data
#' post_util = subset(retrofit_util, retrofit_util$prepost == 3)#subset post-retrofit utility data
#' post_result = run_model(post_util) #result for post-retrofit data}
run_model <- function(util, plot_flag = FALSE, step = 0.5, n = 4, unretrofit_flag = TRUE)
{	
	#require(plotly)
	options(digits=15)

	if(check_zeros(util)){stop("More than half of usage (dependent variables)
							data points of input dataframe are zero.")}
	final_best = list()

	if (is.null(util$bdbid))
	{
		b_name = ''
	}else
	{
		b_name = unique(util$bdbid)
	}

	energy = unique(util$energy_type)

	x = util$OAT
	y = util$usage
	if (is.null(util$estimated))
	{
		z = rep(1, length(x))
		util$estimated = z
	}else
	{
		z = c(1:length(x))
		z[util$estimated == 1] = 'Est'
		z[util$estimated != 1] = 'Act'
		z = z[sort.list(x)]
	}

	model_list = c('2P', '3PC', '3PH', '4P', '5P')
	temp = sort_matrix(x,y)
	x = temp$x
	y = temp$y
	n = length(x)
	step_char = as.character(length(step))

	for (model in model_list)
	{	
		bestvalue = switch(step_char, '1' = create_model(x,y,model, step = step),
			create_model_2(x,y,model, step = step))
		bestvalue[['stat_test']]= model_test(x, y, bestvalue, model, n) 
		if (plot_flag){bestvalue[['figure']] = main_plot_handler(util, bestvalue, energy, b_name, unretrofit_flag)}
		final_best[[model]] = bestvalue
	}
	
	return(final_best)
}


#' tTest
#' 
#' This function calcuates t-stats of slopes. This function is called inside \code{model_test}. The threshold for tstat is set to 2 standard deviation from the null hypothesis where the slope is equal to zero.
#' @param x A vector. Independent variables.
#' @param y A vector. Dependent variables.
#' @param bestvalue A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param model A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.
#' @export
#' @seealso \code{\link{pop_test}} and \code{\link{shape_test}}
#' @examples
#' x = matrix(c(1:10))
#' y = matrix(c(1,2.5,3,4,5,5.5,7,8.3,9,10))
#' model = '4P'
#' bestvalue = create_model(x, y, model)
#' t_matrix = tTest(x, y, bestvalue, model)
tTest <- function(x, y, bestvalue, model)
{	
	options(digits=15)
	cp1 = bestvalue$cp1
	cp2 = bestvalue$cp2
	RMSE = as.numeric(bestvalue$stats['RMSE',1])
	answer = 'Fail'

	t_val = 2.0
	tstat1 = 0.0
	tstat2 = 0.0

	y_predict = y_gen(model, x, bestvalue$params, bestvalue$cp1, bestvalue$cp2)

	if (model == '2P')
	{
		answer = 'Pass'
	}else if (model == '3PC' | model == '3PH')
	{
		tstat1 = switch(as.character(model),
			'3PC' = bestvalue$params['Slope1',1]/calc_std_error(x, y, y_predict, bestvalue$cp1, 'right'),
			'3PH' = bestvalue$params['Slope1',1]/calc_std_error(x, y, y_predict, bestvalue$cp1, 'left'))
		if (abs(tstat1) > t_val){answer = 'Pass'}
	}else
	{	tstat1 = bestvalue$params['Slope1',1]/calc_std_error(x, y, y_predict, bestvalue$cp1, 'left')
		tstat2 = switch(as.character(model),
				'4P' = bestvalue$params['Slope2',1]/calc_std_error(x, y, y_predict, bestvalue$cp1, 'right'),
				'5P' = bestvalue$params['Slope2',1]/calc_std_error(x, y, y_predict, bestvalue$cp2, 'right'))
		
		if (abs(tstat2) > t_val & abs(tstat1) > t_val){answer = 'Pass'}
	}

	t_matrix = matrix(c(answer, tstat1, tstat2))
	row.names(t_matrix) = c('answer','tstat1','tstat2')
	return(t_matrix)
}

#' Polpulation test
#' 
#' This function is a population test function and returns a matrix of 'Pass' or 'Fail', depending on the values of the slopes and models, and number of heating month and cooling months. The threshold hold is \code{length(x)/4}.
#' @param x A vector. Independent variables.
#' @param bestvalue A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param model A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'
#' @param n A numeric value that determines threshold: \code{thereshold = length(x)/n}. Defaults to 4.
#' @export
#' @seealso \code{\link{tTest}} and \code{\link{shape_test}}
#' @examples
#' x = matrix(c(1:10))
#' y = matrix(c(1,2.5,3,4,5,5.5,7,8.3,9,10))
#' model = '4P'
#' bestvalue = create_model(x, y, model)
#' pop_matrix = pop_test(x, bestvalue, model)
pop_test <- function(x, bestvalue, model, n = 4)
{
	cp1 = bestvalue$cp1
	cp2 = bestvalue$cp2
	threshold = (length(x)/4)

	temp = c(1:length(x))
	temp[x <= cp1] <- 1
	temp[x > cp1] <- 0
	heatnum = sum(temp)

	temp = c(1:length(x))
	if(model=='2P'){
		options(warn=-1)
	}
	temp[x >= max(cp1,cp2, na.rm =TRUE)] <- 1
	temp[x < max(cp1,cp2, na.rm =TRUE)] <- 0
	options(warn=0)
	coolnum = sum(temp)

	answer = switch(model, 
		"2P" = {heatnum = NA
			if (coolnum >= threshold){"Pass"}else{'Fail'}
		},
		"3PC" = {
			heatnum = NA
			if (coolnum >= threshold & (length(x) - coolnum) >= threshold){'Pass'}else{'Fail'}
		},
		"3PH" = {
			coolnum = NA
			if ( heatnum >= threshold & (length(x)-heatnum) >= threshold){'Pass'}else{'Fail'}
		},
		"4P" = {
			if (coolnum >= threshold & heatnum >= threshold){'Pass'}else{'Fail'}
		},
		"5P" = {
				if (coolnum >= threshold & heatnum >= threshold
					& (length(x) - (heatnum + coolnum)) >= threshold){'Pass'}else{'Fail'}
		}
		)

	pop_matrix = matrix(c(answer,heatnum, coolnum))
	row.names(pop_matrix) = c('answer','heat_months','cool_months')
	return(pop_matrix)
}

#' Shape test
#' 
#' This function tests if a given model has an expected shape by evaluating the slopes (positive, negative or zero).
#' 
#' @param bestvalue A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param model A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.
#' @export
#' @seealso \code{link{tTest}} and \code{\link{pop_test}}
#' @examples
#' x = matrix(c(1:10))
#' y = matrix(c(1,2.5,3,4,5,5.5,7,8.3,9,10))
#' model = '4P'
#' bestvalue = create_model(x, y, model)
#' shape_test(bestvalue, model)
shape_test <- function(bestvalue, model)
{
	Slope1 = bestvalue$params['Slope1',1]
	Slope2 = if(length(bestvalue$params)==3){bestvalue$params['Slope2',1]}else{0}

	answer = switch(model,
		"2P" = 'Pass',
		"3PC" = if (Slope1 > 0.0){'Pass'}else{'Fail'},
		"3PH" = if (Slope1 < 0.0){'Pass'}else{'Fail'},
		"4P" = {
					if (Slope1 > 0.0 & Slope2 < 0.0)
					{
						'Fail'
					}else if (Slope1 < 0.0 & Slope2 > 0.0)
					{
						'Pass'
					}else if (Slope1 < 0.0)
					{
						if(abs(Slope1) > abs(Slope2)){'Pass'}else{'Fail'}
					}else if (abs(Slope1) > abs(Slope2))
					{
						'Fail'
					}else
					{
						'Pass'
					}
				},
		"5P" = if(Slope1 < 0.0 & Slope2 > 0.0){'Pass'}else{'Fail'}
		)

	shape_matrix = matrix(c(answer))
	row.names(shape_matrix) = c('answer')
	return(shape_matrix)
}

#' Tests if a model passes tTest, population test, and shape test
#' 
#' This function determines if a given model passes tTest, population test, and shape test. The output is a nested list. If the model passes all three tests, 'Pass' is returned as \code{main_test} result; otherwise, as 'Fail'.
#' @param x A vector. Independent variables.
#' @param y A vector. Dependent variables.
#' @param bestvalue A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param model A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.
#' @param n A numeric value that determines threshold for population test: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.
#' @export
#' @examples
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec')
#' temp = sort_matrix(util$OAT,util$usage)
#' bestvalue = create_model(temp$x, temp$y, '5P')
#' test_result = model_test(temp$x, temp$y, bestvalue, '5P')
model_test <- function(x, y, bestvalue, model, n = 4)
{	
	test_result = list(main_test = NULL)
	sum = 0
	test_result[['tTest']] = tTest(x, y, bestvalue, model)
	test_result[['pop_test']] = pop_test(x, bestvalue, model, n)
	test_result[['shape_test']] = shape_test(bestvalue, model)

	for (i in c('tTest', 'pop_test', 'shape_test'))
	{
		if (test_result[[i]][1,1] == 'Pass'){count = 1}else{count =0}
		sum = sum+count
	}

	if(sum == 3){test_result$main_test = 'Pass'}else{test_result$main_test = 'Fail'}

	return(test_result)
}

#' Generates comprehensive output for AIC and BIC
#' 
#' This function generates comprehensive output for the aic and bic statistical tests. 
#' It includes information such as the best model (that have the smallest aic and bic values) and the aic and bic values for each parameter model type.
#' @param final_best A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param sample_number Length of the data frame/number of temperature datapoints.
#' @export
#' @examples
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec')
#' sample_number = nrow(util)
#' result = run_model(util)
#' aic_bic_values = aic_bic_model(result, sample_number)
aic_bic_model <- function(final_best, sample_number)
{
	model_vec  = c('2P', '3PC', '3PH', '4P', '5P')
	aic_vec = c()
	bic_vec = c()
	aic_prob_vec = c()
	for (model in model_vec)
	{
		bestvalue = final_best[[model]]
		aic_bic_vec = aic_bic_test(model, bestvalue, sample_number)

		AIC = aic_bic_vec[1]
		aic_vec = c(aic_vec, AIC)
		aic_min = min(aic_vec)
		if (aic_min == AIC ){aic_model = model}

		BIC = aic_bic_vec[2]
		bic_vec = c(bic_vec, BIC)
		bic_min = min(bic_vec)
		if (bic_min == BIC ){bic_model = model}
	}
	aic_min = min(aic_vec)
	for (i in aic_vec)
	{
		aic_prob = exp((aic_min - i)/2)
		aic_prob_vec = c(aic_prob_vec, aic_prob)
	}
	bic_matrix = matrix(c(bic_vec))
	row.names(bic_matrix) = model_vec
	bic_min_matrix = matrix(c(bic_model, bic_min))
	row.names(bic_min_matrix) = c('bic_min_model', 'bic_min')

	aic_matrix = matrix(c(aic_vec))
	row.names(aic_matrix) = model_vec
	aic_min_matrix = matrix(c(aic_model, aic_min))
	row.names(aic_min_matrix) = c('aic_min_model', 'aic_min')
	aic_prob_matrix = matrix(aic_prob_vec)
	row.names(aic_prob_matrix) = model_vec
	
	aic_bic_summary = list(bic_matrix = bic_matrix, bic_min = bic_min_matrix,
					aic_matrix = aic_matrix, aic_min = aic_min_matrix, aic_prob = aic_prob_matrix)
	return(aic_bic_summary)
}


#' Calculates AIC and BIC
#' 
#' This function generates numeric output for the aic and bic test statistical test. The ouput is a vector and the first value is AIC and the second BIC.
#' @param model A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.
#' @param bestvalue A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param sample_number Length of the data frame/number of temperature datapoints.
#' @export
#' @examples
#' \dontrun{
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec')
#' sample_number = nrow(util)
#' model = '4P'
#' temp = sort_matrix(util$OAT,util$usage)
#' bestvalue = create_model(temp$x, temp$y, '5P')
#' aic_bic_values = aic_bic_test(model, bestvalue, sample_number)}
aic_bic_test <- function(model, bestvalue, sample_number)
{
	if (model == '2P')
	{
		k = 2
	}else if(model == '3PC' | model == '3PH')
	{
		k = 3
	}else if (model == '4P')
	{
		k = 4
	}else
	{	
		k = 5
	}
	RSS = as.numeric(bestvalue$stats['SSE',1])
	AIC = 2*k + (sample_number*log(RSS))
	BIC = (sample_number*k)+ (sample_number*log(RSS/sample_number))
	aic_bic_vec = c(AIC, BIC)
	return(aic_bic_vec)
}


#' Returns the best model
#' 
#' This function returns the best model if there is one. Otherwise, returns an empty list.
#' @param inter_result A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param metric_vec A numeric (integer) vector. This vector determines the pirority of model types. Defaults to \code{c(1,2,3,4,5)} for models: '2P', '3PC', '3PH', '4P', '5P'.
#' @param CV_RMSE_n A numeric value. CV-RMSE threshold value. Defaults to 0.25.
#' @param Rsquared_n A numeric value. Rsquared threshold value. Defaults to 0.75.
#' @return A list with components:
#' \item{stats}{A matrix of statistical results such as RMSE, R-squared}
#' \item{params}{A matrix of parameters such as basload and slopes}
#' \item{cp1}{Change point. The leftmost change point. This will be zero if \code{model} is '2P'.}
#' \item{cp2}{Change point. The rightmost change point. This will be zero if \code{model} is not '5P'.}
#' \item{model}{Model type.}
#' \item{stat_test$main_test}{Main(final) test result. Will be 'Fail' if the model fails one of tTest, population test or shape test. Else, 'Passs'.}
#' \item{stat_test$tTest}{t-test results}
#' \item{stat_test$pop_test}{population test results}
#' \item{stat_test$shape_test}{shape test results}
#' \item{figure}{A parameter graph.}
#' @export
#' @seealso \code{\link{run_model}}, and \code{\link{run_model_retrofit}}
#' @examples
#' \dontrun{
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec') #unretrofit data
#' result = run_model(util)
#' best_result = main_best_model_func(result, 'Elec') #best_model for unretrofit data
#' retrofit_util = subset(retrofit_utility, retrofit_utility$bdbid == 846152
#'						& retrofit_utility$energy_type == 'Elec') #retrofit data
#' retrofit_result = run_model_retrofit(retrofit_util)
#' retrofit_result$pre #result for pre-retrofit
#'
#' #best_model for pre-retrofit data
#' retrofit_best_result = main_best_model_func(retrofit_result$pre, 'Elec')}
main_best_model_func <- function(inter_result, metric_vec = c(1,2,3,4,5), CV_RMSE_n = 0.25, Rsquared_n = 0.75)
{
  best_result = model_pass_func(inter_result, CV_RMSE_n, Rsquared_n)

  metric_df = data.frame(model_type = c('2P','3PC','3PH','4P','5P'), scores = metric_vec)
 
  if (length(best_result))
  {
    best_model = choosing_final_best(best_result, metric_df)
  }else
  {
    best_model = list()
  }

  return(best_model)
}

#' Filters for models that have satisfying threshold
#' 
#' @description
#' This function returns a list which contains models that have satisfying threshold. Defualts threshold: CV-RMSE <= 0.25 and r-squared >= 0.75, and Pass all of shape test, tTest and population test. Suggested Threshold value for fuel is CV-RMSE <= 0.50 and r-squared >= 0.75; for elec, one can use default value.
#' @param inter_result A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param CV_RMSE_n A numeric value. CV-RMSE threshold value. Defaults to 0.25.
#' @param Rsquared_n A numeric value. Rsquared threshold value. Defaults to 0.75.
#' @export
#' @examples
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec')
#' result = run_model(util)
#' best_result = model_pass_func(result)
model_pass_func <- function(inter_result, CV_RMSE_n = 0.25, Rsquared_n = 0.75)
{ 
  best_result = list()
  
    model_list = c('5P', '4P', '3PC', '3PH', '2P')
    for (model in model_list)
    {
      if (inter_result[[model]]$stat_test$main_test == 'Pass'
      		& .subset2(inter_result[[model]]$stats, 3)[1] <= CV_RMSE_n
      		& .subset2(inter_result[[model]]$stats, 2)[1] >= Rsquared_n)
      { 
        best_result[[model]] = inter_result[[model]]
      }
    }
  return(best_result)
}



#' Chooses the best parameter model
#' 
#' This function returns the best model.
#' @param best_result A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param metric_df A data frame with columns "model_type" (string) and "scores" (numeric/integer). The score of a given model determines the pirority of that model type.
#' @export
#' @examples
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86' 
#'				& unretrofit_utility$energy_type == 'Elec')
#' result = run_model(util)
#' best_result = model_pass_func(result)
#' metric_df = data.frame(model_type = c('2P','3PC','3PH','4P','5P'), scores = c(1,2,3,4,5))
#' best_model = choosing_final_best(best_result, metric_df)
choosing_final_best <- function(best_result, metric_df)
{	
  pass_model_vec = c()
  for (i in 1:length(best_result)){
  	pass_model_vec = c(pass_model_vec, best_result[[i]]['model'])
  }
  metric_df = subset(metric_df, metric_df$model_type %in% pass_model_vec)
  best_model_type = subset(metric_df$model_type, max(as.vector(metric_df$score)) == metric_df$score)
  best_result = best_result[[as.character(best_model_type)]]
  return(best_result)
}


#' Calculates normalized annual comsumption
#'
#' This function calculates normalized annual consumption.
#' @param best_result A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param norm_temp A data frame of normalized average monthly temperature with columns: month and avg_temp. Default to \code{\link{norm_temp_tmy3}}.
#' @export
calc_nac <- function(best_result, norm_temp = NULL)
{	
	if(is.null(norm_temp))
	{
		norm_temp = bRema::norm_temp_tmy3
	}
	return(sum(y_gen(best_result$model, norm_temp$avg_temp, best_result$params,
						cp1 = best_result$cp1, cp2 = best_result$cp2)))
}

#' Batch run for specific energy (unretrofit)
#'
#' This function run batches given given a utility data frame of \strong{unretrofit data} and energy. See \code{\link{unretrofit_utility}.}
#' @param utility A utility data frame of \strong{unretrofit data} with multiple buildings. The data frame must have columns: OAT, usage and estimated
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @param metric_vec A numeric (integer) vector. This vector determines the pirority of model types. Defaults to \code{c(1,2,3,4,5)} for models: '2P', '3PC', '3PH', '4P', '5P'.
#' @param plot_flag A boolean value. Defaults to \code{FALSE}. If set to \code{FALSE}, it will not return any plots.
#' @param step A single numeric value or a vector. If it is a single numeric value, \code{create_model} will be used to find change-point(s). If it is a vector with more than one component, \code{create_model_2} will be used. Defaults to 0.5. Adjust \code{step}-size depending on independent varaiables.
#' @param CV_RMSE_n A numeric value. CV-RMSE threshold value. Defaults to 0.25.
#' @param Rsquared_n A numeric value. Rsquared threshold value. Defaults to 0.75.
#' @param n A numeric value that determines threshold for population test: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.
#' @param all_model_flag A boolean value. Defaults to \code{FALSE}. If set to \code{TRUE}, information of all models (i.e not just the best model) of a building will be put into a dataframe.
#' @return A list with components:
#' \item{best_result_df}{A data frame of best models of multiple buildings. If a building does not model, it will not be shown in this data frame.}
#' \item{plot_list}{A list containing plots. If \code{plot_flag} is set to \code{FALSE}, this will be an empty list.}
#' \item{all_model_df}{A data frame of all models of multiple buildings. If \code{all_model_flag} is set to \code{FALSE}, this will be an empty data frame.}
#' @export
#' @examples
#' \dontrun{
#' batch_result = batch_run_energy(unretrofit_utility, 'Elec')
#' }
batch_run_energy <- function(utility, energy, metric_vec = c(1,2,3,4,5),
							plot_flag = FALSE, step = 0.5,
							CV_RMSE_n = 0.25, Rsquared_n = 0.75,
							n = 4, all_model_flag = FALSE)
{
  utility = subset(utility, utility$energy_type == energy)
  plot_list = list()
  best_result_df = data.frame()
  all_model_df = data.frame()
  fiscal_year = max(unique(utility$fiscal_year))
  for (bdbid_n in unique(utility$bdbid))
  {	
    util = subset(utility, utility$bdbid == bdbid_n)
    points = nrow(util)
    if(check_zeros(util)){
    	warning(paste('More than half of', energy,'usage points of', bdbid_n,'are zeros. Thus, it is being skipped.'))
    	next
   	}
    inter_result = run_model(util, plot_flag = plot_flag, step = step, n = n)
	print(inter_result)

    if(all_model_flag)
    {
    	temp_all_model_df = construct_all_model(inter_result, bdbid_n, energy,
    											fiscal_year, points,
    											prepost = 1)
    	all_model_df = rbind(all_model_df, temp_all_model_df)
    }

    best_result = main_best_model_func(inter_result, metric_vec, CV_RMSE_n,Rsquared_n)
    if (!(length(best_result) == 0))
    {
      if (plot_flag)
      {
      	plot_list[[energy]][[as.character(bdbid_n)]] = best_result$figure
      }

      if(all_model_flag)
      {	
      	temp = subset(temp_all_model_df, temp_all_model_df$model_type == best_result$model)
      }else
      {
      	temp = construct_model_table(best_result, bdbid_n, energy, fiscal_year, points,
      								prepost = 1)
      }
      best_result_df = rbind(best_result_df, temp)
    }
  }
  return(list(best_result_df = best_result_df, plot_list = plot_list, all_model_df = all_model_df))
}

#' Batch run for both energy type (unretrofit)
#'
#' This function run batches given a utility data frame of \strong{unretrofit data}. See \code{\link{unretrofit_utility}.}
#' @param utility A utility data frame of \strong{unretrofit data}with multiple buildings.
#' @param metric_elec A numeric (integer) vector. This vector determines the pirority of model types for elec. Defaults to \code{c(1,2,3,4,5)} for models: '2P', '3PC', '3PH', '4P', '5P'.
#' @param metric_fuel A numeric (integer) vector. This vector determines the pirority of model types for fuel. Defaults to \code{c(1,5,4,3,2)} for models: '2P', '3PC', '3PH', '4P', '5P'.
#' @param plot_flag A boolean value. Defaults to \code{FALSE}. If set to \code{FALSE}, it will not return any plots.
#' @param step A single numeric value or a vector. If it is a single numeric value, \code{create_model} will be used to find change-point(s). If it is a vector with more than one component, \code{create_model_2} will be used. Defaults to 0.5. Adjust \code{step}-size depending on independent varaiables.
#' @param CV_RMSE_elec A numeric value. CV-RMSE threshold value for Elec. Defaults to 0.25.
#' @param Rsquared_elec A numeric value. Rsquared threshold value for Elec. Defaults to 0.75.
#' @param CV_RMSE_fuel A numeric value. CV-RMSE threshold value for Fuel. Defaults to 0.50.
#' @param Rsquared_fuel A numeric value. Rsquared threshold value for Fuel. Defaults to 0.75.
#' @param n A numeric value that determines threshold for population test: \code{thereshold = number_of_independent_variables/n}. Defaults to 4. See \code{\link{pop_test}}.
#' @param all_model_flag A boolean value. Defaults to \code{FALSE}. If set to \code{TRUE}, information of all models (i.e not just the best model) of a building will be put into a dataframe.
#' @return A list with components:
#' \item{best_result_df}{A data frame of best models of mulitple buildings. If a building does not model, it will not be shown in this data frame.}
#' \item{plot_list}{A list containing plots. If \code{plot_flag} is set to \code{FALSE}, this will be an empty list.}
#' \item{all_model_df}{A data frame of all models of multiple buildings. If \code{all_model_flag} is set to \code{FALSE}, this will be an empty data frame.}
#' @export
#' @section Note:
#' If more than half of energy usage points of a facility are zeros, that faciltiy will be skipped.
#' @examples
#' \dontrun{
#' batch_result = batch_run(unretrofit_utility)
#' }
batch_run <- function(utility, metric_elec = c(1,2,3,4,5), metric_fuel = c(1,5,4,3,2),
						plot_flag = FALSE, step = 0.5,
						CV_RMSE_elec = 0.25, Rsquared_elec = 0.75, CV_RMSE_fuel = 0.50, 
						Rsquared_fuel = 0.75, n =4, all_model_flag = FALSE)
{ 
  result = list(best_result_df = data.frame(), plot_list = list(), all_model_df = data.frame())
  for (energy in as.character(unique(utility$energy_type)))
  {	

  	metric_vec = switch(energy, 'Elec' = metric_elec, 'Fuel' = metric_fuel)
  	CV_RMSE_n = switch(energy, 'Elec' = CV_RMSE_elec, 'Fuel' = CV_RMSE_fuel)
  	Rsquared_n = switch(energy, 'Elec' = Rsquared_elec, 'Fuel' = Rsquared_fuel)

    temp = batch_run_energy(utility, energy, metric_vec = metric_vec,
    						plot_flag = plot_flag, step = step, CV_RMSE_n = CV_RMSE_n,
    						 Rsquared_n = Rsquared_n, n, all_model_flag)
    result$best_result_df = rbind(result$best_result_df,temp$best_result_df)
    result$plot_list = append(result$plot_list, temp$plot_list)
    if (all_model_flag)
    {	
    	result$all_model_df = rbind(result$all_model_df, temp$all_model_df)
    }
  }

  if(all_model_flag)
  {
  	result$all_model_df = result$all_model_df[order(result$all_model_df[,'bdbid'],
                                  result$all_model_df[,'energy_type']),]
  	rownames(result$all_model_df) = c(1:nrow(result$all_model_df))
  }

  if(is.null(result$best_result_df) | length(result$best_result_df) == 0)
  {	
  	print("No faciltiy is modelled. Returning NULL.")
  	print("If you have set all_model_flag to TRUE, see all_model_df for more information about models.")
  	return(result)
  }

  result$best_result_df = result$best_result_df[order(result$best_result_df[,'bdbid'],
                                  result$best_result_df[,'energy_type']),]
  rownames(result$best_result_df) = c(1:nrow(result$best_result_df))
  return(result)
}

#' Construct model table
#'
#' This functions returns a data frame given a nested list of individual model result.
#'
#' @param best_result A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @param bdbid Building id, which identifies the building or the dataset. Defaults to \code{NA}.
#' @param energy A character string. Energy Type, either 'Elec' or 'Fuel'.
#' @param fiscal_year Fiscal year. Defaults to \code{NA}.
#' @param n number of observed points (independent variable) in utility data frame of a building. Defaults to \code{NA}.
#' @param prepost A numeric value. Defaults to be 1. This indicates if it is unretrofit or retrofit. See \code{\link{unretrofit_utility}} for more information about prepost.
#' @export
#' @section Note:
#' If more than half of energy usage points of a facility are zeros, that faciltiy will be skipped.
#' @examples
#' 
#' util = subset(unretrofit_utility, unretrofit_utility$bdbid == 'f3acce86'
#'				& unretrofit_utility$energy_type == 'Elec')
#' inter_result = run_model(util)
#' best_result = main_best_model_func(inter_result)
#' construct_model_table(best_result, 'f3acce86', 'Elec', 2017)
#' construct_model_table(inter_result[['3PC']], 'f3acce86', 'Elec', 2017)
construct_model_table <- function(best_result, bdbid = NA, energy, fiscal_year = NA, n = NA, prepost = 1)
{ 
  df = data.frame(bdbid = bdbid, energy_type = energy, model_type = best_result$model, n = n)
  df = cbind(df,collect_slopes(best_result))
  df$prepost = prepost
  df$fiscal_year = fiscal_year
  df$xcp1 = best_result$cp1
  df$xcp2 = best_result$cp2
  df$ycp = .subset2(best_result$params, 1)[1]
  df$nac = calc_nac(best_result)
  df$cool_months = .subset2(best_result$stat_test$pop_test, 3)[1]
  df$heat_months = .subset2(best_result$stat_test$pop_test, 2)[1]
  df$r2 = .subset2(best_result$stats, 2)[1]
  df$cv_rmse = .subset2(best_result$stats, 3)[1]
  df$rmse = .subset2(best_result$stats, 1)[1]
  df$shape_test = .subset2(best_result$stat_test$shape_test, 1)[1]
  df$data_post_test = .subset2(best_result$stat_test$pop_test, 1)[1]
  df$t_stat_test = .subset2(best_result$stat_test$tTest, 1)[1]
  df$main_test = best_result$stat_test$main_test
  return(df)
}

#' Return a data frame of slopes
#'
#' This function extracts and returns a datamframe of slopes given a nested list of individual model result. 
#' @param best_result A list containing information about parameters such as slopes, change-points, and stats such as RMSE.
#' @export
collect_slopes <- function(best_result)
{ 
  df = switch(best_result$model, 
    "2P" = {
      data.frame(ls = .subset2(best_result$params, 2)[1],
          ls_t = .subset2(best_result$stat_test$tTest, 2)[1],
          rs = NA,
          rs_t = NA)
    },
    "3PC" ={
      data.frame(
        ls = 0,
        ls_t = NA,
        rs = .subset2(best_result$params, 2)[1],
        rs_t = .subset2(best_result$stat_test$tTest, 2)[1])
    },
    "3PH" = {
      data.frame(
        ls = .subset2(best_result$params, 2)[1],
        ls_t = .subset2(best_result$stat_test$tTest, 2)[1],
        rs = 0,
        rs_t = NA)
    },
    "4P" = {
      data.frame(
        ls = .subset2(best_result$params, 2)[1],
        ls_t = .subset2(best_result$stat_test$tTest, 2)[1],
        rs = .subset2(best_result$params, 3)[1],
        rs_t = .subset2(best_result$stat_test$tTest, 3)[1])
    },
    "5P" = {
      data.frame(
        ls = .subset2(best_result$params, 2)[1],
        ls_t = .subset2(best_result$stat_test$tTest, 2)[1],
        rs = .subset2(best_result$params, 3)[1],
        rs_t = .subset2(best_result$stat_test$tTest, 3)[1])
    }
    )
  return(df)
}

check_zeros <- function(df)
{
	org_len = nrow(df)/2
	zero_len = length(df$usage[df$usage == 0])
	if (org_len > zero_len)
	{
		return(FALSE)
	}else
	{
		return(TRUE)
	}
}

calc_std_error <- function(x_0, y, y_predict, cp, left_right = 'left')
{	
	if (left_right == 'left'){
		x = x_0[x_0 <= cp]
		y = y[x_0 <= cp]
		y_predict = y_predict[x_0 <= cp]
	}else if (left_right == 'right'){
		x = x_0[x_0 >= cp]
		y = y[x_0 >= cp]
		y_predict = y_predict[x_0 >= cp]
	}else{
		x = x_0
	}

	SSE = sum((y - y_predict)^2)
	numerator = sqrt(SSE/((length(y) - 2)))
	denominator = sqrt(sum((x-mean(x))^2))
	SE = numerator/denominator
	return(SE)
}