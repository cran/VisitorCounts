% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_visitation_model.R
\name{fit_model}
\alias{fit_model}
\title{Fit Model}
\usage{
fit_model(
  parameter_estimates_and_time_series_windows,
  omit_trend,
  trend,
  is_input_logged,
  ...
)
}
\arguments{
\item{parameter_estimates_and_time_series_windows}{# a list storing the outputs of \code{\link{estimate_parameters}}, including parameter estimates `constant`, `beta`, and `slope`, as well as data pertaining to time series windows.}

\item{omit_trend}{This is obsolete and is left only for compatibility. In other words, \code{trend} will overwrite any option chosen in \code{omit_trend}. If \code{trend} is NULL, then \code{trend} is overwritten according to \code{omit_trend}. It is a Boolean specifying whether or not to consider the trend component to be 0. The default option is TRUE, in which case, the trend component is 0. If it is set to FALSE, then it is estimated using data.}

\item{trend}{A character string specifying how the trend is modeled. Can be any of NULL, "linear", "none", and "estimated", where "none" and "estimated" correspond to \code{omit_trend} being TRUE and FALSE, respectively. If NULL, then it follows the value specified in \code{omit_trend}.}

\item{is_input_logged}{a Boolean specifying if the input is logged or not.}

\item{...}{Additional arguments}
}
\value{
\item{visitation_fit}{A vector storing fitted values of visitation model.}
}
\description{
Fit the visitation model.
}
