% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modeller.R
\name{splitter}
\alias{splitter}
\title{Splits and restructures a matrix}
\usage{
splitter(model, xmat, cp1 = NA, cp2 = NA)
}
\arguments{
\item{model}{A character string. Model such as '2P', '3PH', '3PC', '4P' or '5P'.}

\item{xmat}{Independent Column matrix (dimesion, n by 1).}

\item{cp1}{A numeric value. The first change-point. Defaults to NA.}

\item{cp2}{A numeric value. The second change-point. Defaults to NA.}
}
\description{
This function produces a matrix based on different change-points inside an interval and models.
For example, if the model is '3PC', then any points in \code{xmat} less than the change-point,
\code{cp1}, will be set to be zero and \code{cp1} will be substracted from the rest of the points.
}
\examples{
x = matrix(c(1,2.3,2.78, 3.4,5.5, 6.1, 7 ,8.4, 9,10))
x_split = splitter('4P', x, 3)
}
