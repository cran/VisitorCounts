#' @title "decomposition" Constructor Function
#' @description Constructs objects of the "decomposition" class.
#' @export
#' @param reconstruction_list A list containing important information about the reconstructed time series. In particular, it contains the reconstructed main trend component, overall trend component, seasonal component for each period specified in suspected_periods, and overall seasonal component.
#' @param grouping_matrix A matrix containing information about the locations of the eigenvalue groups for each period in suspected_periods and trend component. The locations are indicated by '1'.
#' @param window_length A numeric indicating the window length.
#' @param ts_ssa An object of the class "ssa".
#' @importFrom methods is
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


#' @title visitation_forecast_ensemble Class
#' @description Class for plotting an array of visitation_foreces
#' @export
#' @param visitation_forecasts An array of  visitation_forecast object
#' @param labels An array of  labels associated with visitation_forecast
#' @importFrom methods is


new_visitation_forecast_ensemble  <- function(visitation_forecasts, labels){
  if (!is.list(visitation_forecasts) ) {stop("visitation_forecasts must be a list of visitation_forecast objects") }
  if(!is.list(labels)) {stop("labels must be a list of strings ") }

  if(length(visitation_forecasts) != length(labels))
  {
      stop("The visitation forecasts and labels must have the same size, please add a label for each corresponding visitation forecast")
  }

  forecast_ensemble<-list()

  for(i in seq_along(visitation_forecasts))
  {

    if(!is(visitation_forecasts[[i]],"visitation_forecast"))
    {
      stop("each visitation_forecast provided must be of type visitation_forecast, please add a name to your visitation_forecast")
    }
    if(!is.character(labels[[i]]))
    {
      stop("each label provided must be string, please add a label to each of the forecasts")
    }

    forecast_ensemble[[i]]<-label_visitation_forecast(visitation_forecasts[[i]], labels[[i]])
  }

  structure(list(forecast_ensemble = forecast_ensemble),
  class = "visitation_forecast_ensemble")
}



#' @return Object of class "visitation_forecast_ensemble".



#' @title labeled_visitation_forecast Class
#' @description Class for visitation_model predictions (for use with predict.visitation_model()).
#' @export
#' @param visitation_forecast A visitation_forecast object
#' @param label A character string of the label of forecast
#' @importFrom methods is

label_visitation_forecast <- function(visitation_forecast, label){
                       
  if(!is(visitation_forecast,"visitation_forecast")) stop("visitation_forecast must be a visitation_forecast object")
  if (!is.character(label)) stop("label must be a string")
  
  structure(list(
                 visitation_forecast = visitation_forecast,
                 label = label), class = "labeled_visitation_forecast")
}

#' @return Object of class "labeled_visitation_forecast".



#' @title visitation_forecast Class
#' @description Class for visitation_model predictions (for use with predict.visitation_model()).
#' @export
#' @param forecasts A time series of forecasts for the visitation model. the forecasts will be in the standard scale of visitors per month
#' @param logged_forecasts A time series of the logged forecasts for the visitation model. 
#' @param differenced_logged_forecasts A time series of the differenced logged forecasts  for the visitation model.
#' @param differenced_standard_forecasts A time series of the exponentiated differenced logged forecasts that are for the visitation model.
#' @param n_ahead An integer describing the number of forecasts made.
#' @param proxy_forecasts A time series of forecasts of the popularity proxy series.
#' @param onsite_usage_forecasts A time series of forecasts of the original time series.
#' @param beta A numeric or a character string specifying the seasonality adjustment factor. (beta_1)
#' @param constant A numeric specifying the constant term in the model. This constant is understood as the mean of the trend-adjusted time_series. (beta_0)
#' @param slope A numeric specifying the slope term in the model when a linear trend is assumed. (beta_2)
#' @param criterion One of "MSE" or "Nonparametric", to specify the criterion used to select the lag.
#' @param past_observations One of "none", "fitted", or "ref_series". If "fitted", past model fitted values are used. If "ref_series", the reference series in the visitation model object is used. Note that if difference = TRUE, one of these is needed to forecast the first difference.
#' @param lag_estimate A numeric value specifying the estimated lag in the visitation model.
#'
#' @return Object of class "Visitation_forecast".



new_visitation_forecast <- function(forecasts,
                                    logged_forecasts, 
                                    differenced_logged_forecasts,
                                    differenced_standard_forecasts,
                                    n_ahead,
                                    proxy_forecasts,
                                    onsite_usage_forecasts,
                                    beta,
                                    constant,
                                    slope,
                                    criterion,
                                    past_observations,
                                    lag_estimate){
  if (!is.ts(forecasts)) stop("visitation_forecast must be a ts object")
  if (!is.ts(logged_forecasts)) stop("logged visitation_forecast must be a ts object")
  if (!is.ts(differenced_logged_forecasts)) stop("differenced logged visitation_forecast must be a ts object")
  if (!is.ts(differenced_standard_forecasts)) stop("differenced standard scale visitation_forecast must be a ts object")
  if (!is.numeric(n_ahead)) stop("n_ahead must be numeric")
  if (!is.list(onsite_usage_forecasts)) stop("time_series_forecasts must be a list")
  structure(list(forecasts = forecasts,
                 logged_forecasts = logged_forecasts, 
                 differenced_logged_forecasts = differenced_logged_forecasts, 
                 differenced_standard_forecasts  = differenced_standard_forecasts,
                 n_ahead = n_ahead,
                 proxy_forecasts = proxy_forecasts,
                 onsite_usage_forecasts = onsite_usage_forecasts,
                 beta = beta,
                 constant = constant,
                 slope = slope,
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
#' @param beta Seasonality adjustment factor. (beta_1)
#' @param constant A numeric describing the constant term used in the model. (beta_0)
#' @param slope A numeric describing the slope term used in the model when trend is set to "linear". (beta_2)
#' @param lag_estimate An integer representing the lag parameter for the model fit.
#' @param onsite_usage_decomposition A decomposition class object representing the decomposition of time series (e.g., park Photo-User-Days).
#' @param proxy_decomposition A decomposition class object representing the decomposition of a popularity measure (e.g., US Photo-User-Days).
#' @param forecasts_needed An integer describing how many forecasts for the proxy_decomposition are needed for the fit.
#' @param ref_series A reference time series (or NULL) used in the model fit.
#' @param criterion A character string specifying the criterion for estimating the lag in popularity_proxy. If "cross-correlation" is chosen, it chooses the lag that maximizes the correlation coefficient between lagged popularity_proxy and onsite_usage. If "MSE" is chosen, it does so by identifying the lagged popularity_proxy whose derivative is closest to that of onsite_usage by minimizing the mean squared error. If "rank" is chosen, it does so by firstly ranking the square errors of the derivatives and identifying the lag which would minimize the mean rank.
#' @param omit_trend This is obsolete and is left only for compatibility. A Boolean specifying whether or not to consider the NPS trend to be zero.
#' @param trend A character string specifying how the trend is modeled. Can be any of NULL, "linear", "none", and "estimated", where "none" and "estimated" correspond to \code{omit_trend} being TRUE and FALSE, respectively. If NULL, then it follows the value specified in \code{omit_trend}.
#' @param call A call for the visitation model.
#'
#' @return  A list of the class "model_forecast".


new_visitation_model <- function(visitation_fit,
                                 differenced_fit,
                                 beta,
                                 constant,
                                 slope,
                                 lag_estimate,
                                 proxy_decomposition,
                                 onsite_usage_decomposition,
                                 forecasts_needed,
                                 ref_series,
                                 criterion,
                                 omit_trend,
                                 trend,
                                 call){

  if (!is.numeric(visitation_fit)) stop("visitation_forecast must be numeric")
  if (!is.numeric(beta)) stop("beta must be numeric")
  if (!is.numeric(constant)) stop("constant must be numeric")
  structure(list(visitation_fit = visitation_fit,
                 differenced_fit = differenced_fit,
                 beta = beta,
                 constant = constant,
                 slope = slope,
                 proxy_decomposition = proxy_decomposition,
                 onsite_usage_decomposition = onsite_usage_decomposition,
                 forecasts_needed = forecasts_needed,
                 lag_estimate = lag_estimate,
                 ref_series = ref_series,
                 criterion = criterion,
                 omit_trend = omit_trend,
                 trend = trend,
                 call = call
                 ),
                class = "visitation_model")
}



