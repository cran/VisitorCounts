#' @title "decomposition" Constructor Function
#' @description Constructs objects of the "decomposition" class.
#' @export
#' @param reconstruction_list A list containing important information about the reconstructed time series. In particular, it contains the reconstructed main trend component, overall trend component, seasonal component for each period specified in suspected_periods, and overall seasonal component.
#' @param grouping_matrix A matrix containing information about the locations of the eigenvalue groups for each period in suspected_periods and trend component. The locations are indicated by '1'.
#' @param window_length A numeric indicating the window length.
#' @param ts_ssa An object of the class "ssa".
#'
#' @return  A list of the class "decomposition".
#'
#'
new_decomposition <- function(reconstruction_list,
                             grouping_matrix,
                             window_length,
                             ts_ssa) {
  if (!is.list(reconstruction_list)) stop("reconstruction_list must be a list")
  if (!is.matrix(grouping_matrix)) stop("grouping_matrix must be a matrix")
  if (!is.numeric(window_length)) stop("window_length must be an integer")
    structure(list(reconstruction = reconstruction_list,
                   grouping = grouping_matrix,
                   window_length_parameter = window_length,
                   ts_ssa = ts_ssa),
              class = "decomposition")
}

#' @title visitation_forecast Class
#' @description Class for visitation_model predictions (for use with predict.visitation_model()).
#' @export
#' @param forecasts A time series of forecasts for the visitation model.
#' @param n_ahead An integer describing the number of forecasts made.
#' @param proxy_forecasts A time series of forecasts of the popularity proxy series.
#' @param onsite_usage_forecasts A time series of forecasts of the original time series.
#' @param beta A numeric or a character string specifying the seasonality adjustment factor. The default option is "estimate", in which case, it is estimated by using the Fisher's z-transformed lag-12 autocorrelation. Even if an actual value is supplied, if ref_series is supplied, it is overwritten by the least squares estimate.
#' @param constant A numeric specifying the constant term in the model. This constant is understood as the mean of the trend-adjusted time_series. The default option is 0, implying that the time_series well represents the actual visitation counts, which is rarely the case. If ref_series is supplied, the constant is overwritten by the least squares estimate.
#' @param criterion One of "MSE" or "Nonparametric", to specify the criterion used to select the lag.
#' @param past_observations One of "none", "fitted", or "ref_series". If "fitted", past model fitted values are used. If "ref_series", the reference series in the visitation model object is used. Note that if difference = TRUE, one of these is needed to forecast the first difference.
#' @param lag_estimate A numeric value specifying the estimated lag in the visitation model.
#'
#' @return Object of class "Visitation_forecast".



new_visitation_forecast <- function(forecasts,
                                    n_ahead,
                                    proxy_forecasts,
                                    onsite_usage_forecasts,
                                    beta,
                                    constant,
                                    criterion,
                                    past_observations,
                                    lag_estimate){

  if (!is.ts(forecasts)) stop("visitation_forecast must be a ts object")
  if (!is.numeric(n_ahead)) stop("n_ahead must be numeric")
  if (!is.list(onsite_usage_forecasts)) stop("time_series_forecasts must be a list")
  structure(list(forecasts = forecasts,
                 n_ahead = n_ahead,
                 proxy_forecasts = proxy_forecasts,
                 onsite_usage_forecasts = onsite_usage_forecasts,
                 beta = beta,
                 constant = constant,
                 criterion = criterion,
                 past_observations = past_observations,
                 lag_estimate = lag_estimate
  ),
  class = "visitation_forecast")
}

#' @title "visitation_model" Constructor Function
#' @description Constructs objects of the "visitation_model" class.
#' @export
#' @param visitation_fit A time series storing the fitted values of the visitation model.
#' @param differenced_fit A time series storing the differenced fitted values of the visitation model.
#' @param beta Seasonality adjustment factor.
#' @param constant A double describing the constant term used in the model.
#' @param lag_estimate An integer representing the lag parameter for the model fit.
#' @param onsite_usage_decomposition A decomposition class object representing the decomposition of time series (e.g., park Photo-User-Days).
#' @param proxy_decomposition A decomposition class object representing the decomposition of a popularity measure (e.g., US Photo-User-Days).
#' @param forecasts_needed An integer describing how many forecasts for the proxy_decomposition are needed for the fit.
#' @param ref_series A reference time series (or NULL) used in the model fit.
#' @param criterion A character string specifying the criterion for estimating the lag in popularity_proxy. If "cross-correlation" is chosen, it chooses the lag that maximizes the correlation coefficient between lagged popularity_proxy and onsite_usage. If "MSE" is chosen, it does so by identifying the lagged popularity_proxy whose derivative is closest to that of onsite_usage by minimizing the mean squared error. If "rank" is chosen, it does so by firstly ranking the square errors of the derivatives and identifying the lag which would minimize the mean rank.
#' @param omit_trend A Boolean specifying whether or not to consider the NPS trend to be zero.
#' @param call A call for the visitation model.
#'
#' @return  A list of the class "model_forecast".


new_visitation_model <- function(visitation_fit,
                                 differenced_fit,
                                 beta,
                                 constant,
                                 lag_estimate,
                                 proxy_decomposition,
                                 onsite_usage_decomposition,
                                 forecasts_needed,
                                 ref_series,
                                 criterion,
                                 omit_trend,
                                 call){

  if (!is.numeric(visitation_fit)) stop("visitation_forecast must be numeric")
  if (!is.numeric(beta)) stop("beta must be numeric")
  if (!is.numeric(constant)) stop("constant must be numeric")
  structure(list(visitation_fit = visitation_fit,
                 differenced_fit = differenced_fit,
                 beta = beta,
                 constant = constant,
                 proxy_decomposition = proxy_decomposition,
                 onsite_usage_decomposition = onsite_usage_decomposition,
                 forecasts_needed = forecasts_needed,
                 lag_estimate = lag_estimate,
                 ref_series = ref_series,
                 criterion = criterion,
                 omit_trend = omit_trend,
                 call = call),
            class = "visitation_model")
}



