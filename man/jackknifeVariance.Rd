% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jackknife_variance.R
\name{jackknifeVariance}
\alias{jackknifeVariance}
\title{Computes jackknife variance}
\usage{
jackknifeVariance(j, kernMatrix, delta, y)
}
\arguments{
\item{j}{integer index for deleted observation in the jackknife procedure.}

\item{kernMatrix}{\code{(n-1)} by \code{m} matrix of kernel values centered at missing
observation \code{j} where \code{n} is the total number of observations and \code{m} is the
number of candidate bandwidths.}

\item{delta}{Binary vector of length \code{n} indicating missingness.
\code{1} where \code{y} is observed and \code{0} where \code{y} is missing.}

\item{y}{numeric vector of length \code{n} of observed values and imputed values.}
}
\value{
returns a single numeric value for the estimate of the jackknife variance.
}
\description{
Computes jackknife variance
}
