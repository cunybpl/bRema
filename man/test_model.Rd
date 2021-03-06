% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modeller.R
\name{test_model}
\alias{test_model}
\title{Performs segmented regression for a given model}
\usage{
test_model(model, x, y, cp1 = NA, cp2 = NA)
}
\arguments{
\item{model}{A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.}

\item{x}{A (column) matrix of outside air temperature, independent variables.}

\item{y}{A (column) matrix of actual values(usage), dependent variables.}

\item{cp1}{A numeric value. The first change-point. Defaults to NA.}

\item{cp2}{A numeric value. The second change-point. Defaults to NA.}
}
\description{
This function returns a list which contains stats matrix, parameter matrix and change-point(s).
}
\examples{
x = matrix(c(1:10))
y = matrix(c(1,2.5,3,4,5,5.5,7,8.3,9,10))
result = test_model('4P', x, y, 4)
}
