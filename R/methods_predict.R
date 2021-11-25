#' @title Predict Decomposition
#' @description Methods for generating predictions from objects of the class "decomposition".
#' @export
#' @param object An object of class "decomposition".
#' @param n_ahead An integer describing the number of forecasts to make.
#' @param only_new A boolean describing whether or not to include past values.
#' @param ... Additional arguments.
#'
#' @return
#' \item{forecasts}{A vector with overall forecast values.}
#' \item{trend_forecasts}{A vector with trend forecast values.}
#' \item{seasonality_forecasts}{A vector with seasonality forecast values.}
#'
#' @examples
#'
#' data("park_visitation")
#' suspected_periods <- c(12,6,4,3)
#' proportion_of_variance_type = "leave_out_first"
#' max_proportion_of_variance <- 0.995
#' log_ratio_cutoff <- 0.2
#'
#' park <- "DEVA"
#'
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, freq = 12)
#' nps_ts <- log(nps_ts)
#'
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, freq = 12)
#' pud_ts <- log(pud_ts)
#'
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, freq = 12)
#' nps_ts <- log(nps_ts)
#'
#' decomp_pud <- auto_decompose(pud_ts,
#'                                      suspected_periods,
#'                                      proportion_of_variance_type = proportion_of_variance_type,
#'                                      max_proportion_of_variance,
#'                                     log_ratio_cutoff)
#' n_ahead = 36
#' pud_predictions <- predict(decomp_pud,n_ahead = n_ahead, only_new = FALSE)
#'
#'
#'


predict.decomposition <- function(object,n_ahead,only_new = TRUE,...){

  ts_ssa <- object$ts_ssa # ssa decomposition of time series
  number_of_components <- dim(object$grouping)[2]

  eigentriple_groupings <- vector(mode = "list", length = number_of_components)

  for(j in 1:number_of_components){
    eigentriple_groupings[[j]] <- which(object$grouping[,j] == 1)
  }

  componentwise_forecasts <- Rssa::rforecast(ts_ssa, groups = eigentriple_groupings, len = n_ahead, only.new = only_new)

  trend_component <- colnames(object$grouping) == "Trend" # a vector whose components are 0 if a column of object$grouping is trend and 1 otherwise

  trend_forecast <- Reduce('+',componentwise_forecasts[trend_component])
  seasonality_forecast <- Reduce('+',componentwise_forecasts[!trend_component]) # sum forecasts corresponding to seasonality


  return(list(forecasts = trend_forecast+seasonality_forecast,
              trend_forecasts = trend_forecast,
              seasonality_forecasts = seasonality_forecast
             ))

}

#' @title Predict Visitation Model
#' @description Methods for generating predictions from objects of the class "visitation_model".
#' @export
#' @param object An object of class "visitation_model".
#' @param n_ahead An integer indicating how many observations to forecast.
#' @param only_new A Boolean specifying whether to include only the forecasts (if TRUE) or the full reconstruction (if FALSE). The default option is TRUE.
#' @param difference A Boolean specifying whether to forecast differences (if TRUE) or the original series (if FALSE). The default option is FALSE.
#' @param past_observations A character string; one of "fitted" or "reference". Here, "fitted" uses the fitted values of the visitation model, while "reference" uses values supplied in `ref_series'.
#' @param ... Additional arguments.
#'
#' @return A predictions for the automatic decomposition.
#' \item{forecasts}{A vector with forecast values.}
#' \item{n_ahead}{A numeric that shows the number of steps ahead.}
#' \item{proxy_forecasts}{A vector for the proxy of trend forecasts.}
#' \item{onsite_usage_forecasts}{A vector for the visitation forecasts.}
#' \item{beta}{A numeric for the seasonality adjustment factor.}
#' \item{constant}{A numeric for the value of the constant in the model.}
#' \item{criterion}{A string which specifies the method used to select the appropriate lag. Only applicable if the trend component is part of the forecasts.}
#' \item{past_observations}{A vector which specifies the fitted values for the past observations.}
#' \item{lag_estimate}{A numeric for the estimated lag. Only applicable if the trend component is part of the forecasts.}
#'
#' @examples
#'
#'data("park_visitation")
#'data("flickr_userdays")
#'
#' n_ahead <- 36
#' park <- "ROMO"
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, frequency = 12)
#' pud_ts <- log(pud_ts)
#'
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, frequency = 12)
#' nps_ts <- log(nps_ts)
#' popularity_proxy <- log(flickr_userdays)
#'
#' vm <- visitation_model(pud_ts,popularity_proxy, ref_series = nps_ts,omit_trend = TRUE)
#' predict_vm <- predict(vm,n_ahead, difference = TRUE,
#'                       only_new = FALSE, past_observations = "reference")
#' plot(predict_vm, difference = FALSE)
#' predict_vm2 <- predict(vm,n_ahead, difference = TRUE,
#'                        only_new = FALSE, past_observations = "reference")
#' plot(predict_vm2, difference = FALSE)


predict.visitation_model <- function(object,
                                     n_ahead,
                                     only_new = TRUE,
                                     difference = FALSE,
                                     past_observations = c("fitted","reference"),
                                     ...){


  past_observations <- match.arg(past_observations)

  omit_trend <- object$omit_trend
  if(omit_trend == TRUE){
    proxy_trend_forecasts <-  NULL
    proxy_trend_correction <- NULL
  }
  if(omit_trend == FALSE){
  proxy_trend_correction <- -object$lag_estimate$lag
  }

  criterion <- object$criterion

  #Forecasts may be needed, or existing values of proxy_forecasts can be used if available.
  time_series_forecasts <- predict(object$onsite_usage_decomposition, n_ahead, only_new = TRUE)

  starttime <- time(time_series_forecasts$forecast)[1]
  endtime <- time(time_series_forecasts$forecast)[length(time_series_forecasts$forecast)]
  ts_frequency <- frequency(time_series_forecasts$forecast)

  if(omit_trend == FALSE){
  proxy_trend_forecasts <- generate_proxy_trend_forecasts(object,n_ahead,starttime,endtime, proxy_trend_correction, ts_frequency)
  forecasts <- log(object$constant)+time_series_forecasts$trend_forecast-proxy_trend_forecasts+object$beta*time_series_forecasts$seasonality_forecast
  }
  if(omit_trend == TRUE){
    # note that object$constant is generated by exp() in visitation_model()
    proxy_trend_forecasts <- numeric(n_ahead)
    forecasts <- log(object$constant)+object$beta*time_series_forecasts$seasonality_forecast
  }


  ### if difference or only_new = F, combine past observations series with forecasts.

  if(identical(past_observations,"fitted")){
    past_values <- object$visitation_fit
  }
  else past_values <- object$ref_series


  if(is.null(past_values)){
    past_values <- object$visitation_fit
    message("Reference series is NULL. Using fitted values instead.")
    }

  if(!only_new){
    past <- past_values
    forecasts <- c(past,forecasts)
  }
  if(difference & only_new){
    past <- past_values[length(object$visitation_fit)]
    forecasts <- c(past,forecasts)
  }



  forecasts <-  ts(forecasts,
                   end = time(object$visitation_fit)[length(object$visitation_fit)]+n_ahead/frequency(object$visitation_fit),
                   frequency = frequency(object$visitation_fit))
  if(difference){
    forecasts <- diff(forecasts)
  }

  if(omit_trend){
    lag_estimate <- NULL
  } else{
    lag_estimate <- -proxy_trend_correction
  }

  return(new_visitation_forecast(forecasts = forecasts,
                                 n_ahead = n_ahead,
                                 proxy_forecasts = proxy_trend_forecasts,
                                 onsite_usage_forecasts = time_series_forecasts,
                                 beta = object$beta,
                                 constant = object$constant,
                                 criterion = criterion,
                                 past_observations = object$ref_series,
                                 lag_estimate = lag_estimate))

}

#' @title Generate Proxy Trend Forecasts
#' @description Generating proxy trend forecasts from objects of the class "visitation_model".
#' @export
#'
#' @param object A visitation model object.
#' @param n_ahead The number of desired forecasts.
#' @param starttime The start time of the desired forecasts.
#' @param endtime The end time of the desired forecasts.
#' @param proxy_trend_correction The lag correction needed on the proxy trend.
#' @param ts_frequency Frequency of the time series to forecast.
#'
#' @return A time series object storing forecasts for the proxy trend.

generate_proxy_trend_forecasts <- function(object,n_ahead,starttime,endtime, proxy_trend_correction, ts_frequency){

  if(object$omit_trend == TRUE){return(numeric(n_ahead))}

  #We adjust to include forecasts needed for the fit itself. Note that negative object$forecasts_needed is possible.
  proxy_forecasts_needed <- n_ahead+object$forecasts_needed

  #Existing proxy_trend values will be used if available. Otherwise, forecasts are needed.
  if(proxy_forecasts_needed > 0){ #If forecasts are needed, use proxy_decomposition to make those forecasts.
    proxy_forecasts <- predict(object$proxy_decomposition, proxy_forecasts_needed, only_new = T)
    proxy_trend_forecasts <- window(stats::lag(proxy_forecasts$trend_forecast, k = proxy_trend_correction),
                                    start = starttime,
                                    end = endtime)

  } else{ #If no forecasts are needed, all of the forecasts can be found in the existing lagged proxy trend.
    proxy_trend_forecasts <- window(stats::lag(object$proxy_decomposition$reconstruction$Trend, k = proxy_trend_correction),
                                    start = starttime,
                                    end = endtime)
  }
  if(object$forecasts_needed < 0){ #Negative forecasts_needed correspond with number of already available observations.
    usable_proxy_trend_observations <- -object$forecasts_needed
    proxy_trend <- object$proxy_decomposition$reconstruction$Trend
    m <- min(usable_proxy_trend_observations,n_ahead)

    proxy_trend_forecasts <- ts(c(proxy_trend[(length(proxy_trend)-usable_proxy_trend_observations+1):(length(proxy_trend)-usable_proxy_trend_observations+m)],proxy_trend_forecasts),
                                start = starttime,
                                end = endtime,
                                frequency = ts_frequency)
  }

  return(proxy_trend_forecasts)

}
