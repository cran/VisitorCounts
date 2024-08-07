% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_visitation_model.R
\name{visitation_model}
\alias{visitation_model}
\title{Visitation Model}
\usage{
visitation_model(
  onsite_usage,
  popularity_proxy = NULL,
  suspected_periods = c(12, 6, 4, 3),
  proportion_of_variance_type = c("leave_out_first", "total"),
  max_proportion_of_variance = 0.995,
  log_ratio_cutoff = 0.2,
  window_length = "auto",
  num_trend_components = 2,
  criterion = c("cross-correlation", "MSE", "rank"),
  possible_lags = -36:36,
  leave_off = 6,
  estimated_change = 0,
  order_of_polynomial_approximation = 7,
  order_of_derivative = 1,
  ref_series = NULL,
  constant = 0,
  beta = "estimate",
  slope = 0,
  is_input_logged = FALSE,
  spline = FALSE,
  parameter_estimates = c("joint", "separate"),
  omit_trend = TRUE,
  trend = c("linear", "none", "estimated"),
  ...
)
}
\arguments{
\item{onsite_usage}{A vector which stores monthly on-site usage for a particular social media platform and recreational site.}

\item{popularity_proxy}{A vector which stores a time series which may be used as a proxy for the monthly popularity of social media over time. The length of \code{popularity_proxy} must be the same as that of \code{onsite_usage}. The default option is NULL, in which case, no proxy needs to be supplied. Note that this vector cannot have a value of 0.}

\item{suspected_periods}{A vector which stores the suspected periods in the descending order of importance. The default option is c(12,6,4,3), corresponding to 12, 6, 4, and 3 months if observations are monthly.}

\item{proportion_of_variance_type}{A character string specifying the option for choosing the maximum number of eigenvalues based on the proportion of total variance explained. If "leave_out_first" is chosen, then the contribution made by the first eigenvector is ignored; otherwise, if "total" is chosen, then the contribution made by all the eigenvectors is considered.}

\item{max_proportion_of_variance}{A numeric specifying the proportion of total variance explained using the method specified in \code{proportion_of_variance_type}. The default option is 0.995.}

\item{log_ratio_cutoff}{A numeric specifying the threshold for the deviation between the estimated period and candidate periods in suspected_periods. The default option is 0.2, which means that if the absolute log ratio between the estimated and candidate period is within 0.2 (approximately a 20 percent difference), then the estimated period is deemed equal to the candidate period.}

\item{window_length}{A character string or positive integer specifying the window length for the SSA estimation. If "auto" is chosen, then the algorithm automatically selects the window length by taking a multiple of 12 which does not exceed half the length of \code{onsite_usage}. The default option is "auto".}

\item{num_trend_components}{A positive integer specifying the number of eigenvectors to be chosen for describing the trend in SSA. The default option is 2. This is relevant only when \code{trend} is "estimated".}

\item{criterion}{A character string specifying the criterion for estimating the lag in \code{popularity_proxy}. If "cross-correlation" is chosen, it chooses the lag that maximizes the correlation coefficient between lagged \code{popularity_proxy} and \code{onsite_usage}. If "MSE" is chosen, it does so by identifying the lagged \code{popularity_proxy} whose derivative is closest to that of \code{onsite_usage} by minimizing the mean squared error. If "rank" is chosen, it does so by firstly ranking the square errors of the derivatives and identifying the lag which would minimize the mean rank.}

\item{possible_lags}{A numeric vector specifying all the candidate lags for \code{popularity_proxy}. The default option is -36:36.  This is relevant only when \code{trend} is "estimated".}

\item{leave_off}{A positive integer specifying the number of observations to be left off when estimating the lag. The default option is 6. This is relevant only when \code{trend} is "estimated".}

\item{estimated_change}{A numeric specifying the estimated change in the visitation trend. The default option is 0, implying no change in the trend.}

\item{order_of_polynomial_approximation}{A numeric specifying the order of the polynomial approximation of the difference between time series used in \code{estimate_lag}. The default option is 7, the seventh-degree polynomial. This is relevant only when \code{trend} is "estimated".}

\item{order_of_derivative}{A numeric specifying the order of derivative for the approximated difference between lagged \code{popularity_proxy} and \code{onsite_usage}. The default option is 1, the first derivative. This is relevant only when \code{trend} is "estimated".}

\item{ref_series}{A numeric vector specifying the original visitation series. The default option is NULL, implying that no such series is available. If such series is available, then its length must be the same as that of \code{onsite_usage}.}

\item{constant}{A numeric specifying the constant term (beta0) in the model. This constant is understood as the mean log adjusted monthly visitation relative to the base month. The default option is 0, implying that the (logged) \code{onsite_usage} does not require any constant shift, which is unusual. If \code{ref_series} is supplied, the constant is overwritten by the least squares estimate.}

\item{beta}{A numeric or a character string specifying the seasonality adjustment factor (beta1). The default option is "estimate", in which case, it is estimated by using the Fisher's z-transformed lag-12 autocorrelation. Even if an actual value is supplied, if \code{ref_series} is supplied, it is overwritten by the least squares estimate.}

\item{slope}{A numeric specifying the slope coefficient (beta2) in the model. This constant is applicable only when \code{trend} is set to "linear". The default option is 0, implying that the linear trend is absent.}

\item{is_input_logged}{A Boolean describing whether the \code{onsite_usage}, \code{ref_series}, and \code{popularity_proxy} are in the log scale. The default option is FALSE, in which case the inputs will be assumed to not be logged and will be logged before making forecasts. Setting it to TRUE will assume the inputs are logged.}

\item{spline}{A Boolean specifying whether or not to use a smoothing spline for the lag estimation. This is relevant only when \code{trend} is "estimated".}

\item{parameter_estimates}{A character string specifying how to estimate beta and constant parameters should a reference series be supplied. Both options use least squares estimates, but "separate" indicates that the differenced series should be used to estimate beta separately from the constant, while "joint" indicates to estimate both using non-differenced detrended series.}

\item{omit_trend}{This is obsolete and is left only for compatibility. In other words, \code{trend} will overwrite any option chosen in \code{omit_trend}. If \code{trend} is NULL, then \code{trend} is overwritten according to \code{omit_trend}. It is a Boolean specifying whether or not to consider the trend component to be 0. The default option is TRUE, in which case, the trend component is 0. If it is set to FALSE, then it is estimated using data.}

\item{trend}{A character string specifying how the trend is modeled. Can be any of NULL, "linear", "none", and "estimated", where "none" and "estimated" correspond to \code{omit_trend} being TRUE and FALSE, respectively. If NULL, then it follows the value specified in \code{omit_trend}.}

\item{...}{Additional arguments to be passed onto the smoothing spline (\code{smooth.spline}).}
}
\value{
\item{visitation_fit}{A vector storing fitted values of visitation model.}
\item{differenced_fit}{A vector storing differenced fitted values of visitation model. (Equal to \code{diff(visitation_fit)}.)}
\item{constant}{A numeric storing estimated constant term used in the model (beta0).}
\item{beta}{A numeric storing the estimated seasonality adjustment factor (beta1).}
\item{slope}{A numeric storing estimated slope coefficient term used in the model (beta2).}
\item{proxy_decomposition}{A "decomposition" object representing the automatic decomposition obtained from \code{popularity_proxy} (see \code{\link{auto_decompose}}).}
\item{time_series_decomposition}{A "decomposition" object representing the automatic decomposition obtained from \code{onsite_usage} (see \code{\link{auto_decompose}}).}
\item{forecasts_needed}{An integer representing the number of forecasts of \code{popularity_proxy} needed to obtain all fitted values. Negative values indicate extra observations which may be useful for predictions.}
\item{lag_estimate}{A list storing both the MSE-based estimate and rank-based estimates for the lag.}
\item{criterion}{A string; one of "cross-correlation", "MSE", or "rank", specifying the method used to select the appropriate lag.}
\item{ref_series}{The reference series, if one was supplied.}
\item{omit_trend}{Whether or not trend was considered 0 in the model. This is obsolete and is left only for compatibility.}
\item{trend}{The trend used in the model.}
\item{call}{The model call.}
}
\description{
Fits a time series model that uses social media posts and popularity of the social media to model visitation to recreational sites.
}
\examples{

### load data --------------------

data("park_visitation")
data("flickr_userdays")

park <- "YELL" #Yellowstone National Park
pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, frequency = 12)
nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, frequency = 12)


### fit three models ---------------

vm_pud_linear <- visitation_model(onsite_usage = pud_ts,
                                  ref_series = nps_ts,
                                  parameter_estimates = "joint",
                                  trend = "linear")
vm_pud_only <- visitation_model(onsite_usage = pud_ts,
                                popularity_proxy = flickr_userdays,
                                trend = "estimated")
vm_ref_series <- visitation_model(onsite_usage = pud_ts,
                                  popularity_proxy = flickr_userdays,
                                  ref_series = nps_ts,
                                  parameter_estimates = "separate",
                                  possible_lags = -36:36,
                                  trend = "none")


### visualize fit ------------------

plot(vm_pud_linear, ylim = c(-3,3), difference = TRUE)
lines(diff(nps_ts), col = "red")

plot(vm_pud_only, ylim = c(-3,3), difference = TRUE)
lines(diff(nps_ts), col = "red")

plot(vm_ref_series, ylim = c(-3,3), difference = TRUE)
lines(diff(nps_ts), col = "red")


}
\seealso{
See \code{\link{predict.visitation_model}} for forecast methods, \code{\link{estimate_lag}} for details on the lag estimation, and \code{\link{auto_decompose}} for details on the automatic decomposition of time series using singular spectrum analysis (SSA). See the package \link[Rssa]{Rssa} for details regarding singular spectrum analysis.
}
