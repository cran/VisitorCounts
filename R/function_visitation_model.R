#' @title Visitation Model
#' @description Fits a time series model that uses social media posts and popularity of the social media to model visitation to recreational sites.
#' @export
#' @param onsite_usage A vector which stores on-site usage in the log scale for a particular social media platform and recreational site.
#' @param popularity_proxy A vector which stores a time series which may be used as a proxy for the social media time series in the log scale. The length of popularity_proxy must be the same as that of onsite_usage. The default option is NULL, in which case, no proxy needs to be supplied.
#' @param suspected_periods A vector which stores the suspected periods in the descending order of importance. The default option is c(12,6,4,3), corresponding to 12, 6, 4, and 3 months if observations are monthly.
#' @param proportion_of_variance_type A character string specifying the option for choosing the maximum number of eigenvalues based on the proportion of total variance explained. If "leave_out_first" is chosen, then the contribution made by the first eigenvector is ignored; otherwise, if "total" is chosen, then the contribution made by all the eigenvectors is considered.
#' @param max_proportion_of_variance A numeric specifying the proportion of total variance explained using the method specified in proportion_of_variance_type. The default option is 0.995.
#' @param log_ratio_cutoff A numeric specifying the threshold for the deviation between the estimated period and candidate periods in suspected_periods. The default option is 0.2, which means that if the absolute log ratio between the estimated and candidate period is within 0.2 (approximately a 20 percent difference), then the estimated period is deemed equal to the candidate period.
#' @param window_length A character string or positive integer specifying the window length for the SSA estimation. If "auto" is chosen, then the algorithm automatically selects the window length by taking a multiple of 12 which does not exceed half the length of onsite_usage. The default option is "auto".
#' @param num_trend_components A positive integer specifying the number of eigenvectors to be chosen for describing the trend in SSA. The default option is 2. This is relevant only when omit_trend is FALSE.
#' @param criterion A character string specifying the criterion for estimating the lag in popularity_proxy. If "cross-correlation" is chosen, it chooses the lag that maximizes the correlation coefficient between lagged popularity_proxy and onsite_usage. If "MSE" is chosen, it does so by identifying the lagged popularity_proxy whose derivative is closest to that of onsite_usage by minimizing the mean squared error. If "rank" is chosen, it does so by firstly ranking the square errors of the derivatives and identifying the lag which would minimize the mean rank.
#' @param possible_lags A numeric vector specifying all the candidate lags for popularity_proxy. The default option is -36:36.  This is relevant only when omit_trend is FALSE.
#' @param leave_off A positive integer specifying the number of observations to be left off when estimating the lag. The default option is 6. This is relevant only when omit_trend is FALSE.
#' @param estimated_change A numeric specifying the estimated change in the visitation trend. The default option is 0, implying no change in the trend.
#' @param order_of_polynomial_approximation A numeric specifying the order of the polynomial approximation of the difference between time series used in \code{estimate_lag}. The default option is 7, the seventh-degree polynomial. This is relevant only when omit_trend is FALSE.
#' @param order_of_derivative A numeric specifying the order of derivative for the approximated difference between time_series1 and lagged time_series2. The default option is 1, the first derivative. This is relevant only when omit_trend is FALSE.
#' @param ref_series A numeric vector specifying the original visitation series in the log scale. The default option is NULL, implying that no such series is available. If such series is available, then its length must be the same as that of time_series.
#' @param beta A numeric or a character string specifying the seasonality adjustment factor. The default option is "estimate", in which case, it is estimated by using the Fisher's z-transformed lag-12 autocorrelation. Even if an actual value is supplied, if ref_series is supplied, it is overwritten by the least squares estimate.
#' @param constant A numeric specifying the constant term in the model. This constant is understood as the mean of the trend-adjusted time_series. The default option is 0, implying that the time_series well represents the actual visitation counts, which is rarely the case. If ref_series is supplied, the constant is overwritten by the least squares estimate.
#' @param log_scale A Boolean specifying whether or not the results should be returned in the log scale. The default option is TRUE, in which case, the results are returned in the log scale.
#' @param spline A Boolean specifying whether or not to use a smoothing spline for the lag estimation. This is relevant only when omit_trend is FALSE.
#' @param parameter_estimates A character string specifying how to estimate beta and constant parameters should a reference series be supplied. Both options use least squares estimates, but "separate" indicates that the differenced series should be used to estimate beta separately from the constant, while "joint" indicates to estimate both using non-differenced detrended series.
#' @param omit_trend A Boolean specifying whether or not to consider the trend component to be 0. The default option is TRUE, in which case, the trend component is 0.
#' @param ... Additional arguments to be passed onto the smoothing spline (\code{smooth.spline}).
#'
#' @return
#' \item{visitation_fit}{A vector storing fitted values of visitation model.}
#' \item{differenced_fit}{A vector storing differenced fitted values of visitation model. (Equal to \code{diff(visitation_fit)}.)}
#' \item{beta}{A numeric storing the estimated seasonality adjustment factor.}
#' \item{constant}{A numeric storing estimated constant term used in the model.}
#' \item{proxy_decomposition}{A "decomposition" object representing the automatic decomposition obtained from popularity_proxy (see auto_decompose())}
#' \item{time_series_decomposition}{A "decomposition" object representing the automatic decomposition obtained from time_series (see auto_decompose())}
#' \item{forecasts_needed}{An integer representing the number of forecasts of popularity_proxy needed to obtain all fitted values. Negative values indicate extra observations which may be useful for predictions.}
#' \item{lag_estimate}{A list storing both the MSE-based estimate and Rank-based estimates for the lag.}
#' \item{criterion}{A string; one of "cross-correlation", "MSE", or "rank", specifying the method used to select the appropriate lag.}
#' \item{ref_series}{The reference series, if one was supplied.}
#' \item{omit_trend}{Whether or not trend was considered 0 in the model.}
#' \item{call}{The model call.}
#'
#' @seealso See \code{\link{predict.visitation_model}} for forecast methods, \code{\link{estimate_lag}} for details on the lag estimation, and \code{\link{auto_decompose}} for details on the automatic decomposition of time series using SSA. See the package \link[Rssa]{Rssa} for details regarding singular spectrum analysis.
#'
#' @examples
#'
#' ### load data --------------------
#'
#' data("park_visitation")
#' data("flickr_userdays")
#'
#' park <- "YELL" #Yellowstone National Park
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, frequency = 12)
#' pud_ts <- log(pud_ts)
#'
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, frequency = 12)
#' nps_ts <- log(nps_ts)
#' popularity_proxy <- log(flickr_userdays)
#'
#'
#' ### fit two models ---------------
#'
#' vm_pud_only <- visitation_model(pud_ts,popularity_proxy = popularity_proxy, omit_trend = FALSE)
#' vm_ref_series <- visitation_model(pud_ts,
#'                                   popularity_proxy,
#'                                   ref_series = nps_ts,
#'                                   parameter_estimates = "separate",
#'                                   possible_lags = -36:36,
#'                                   omit_trend = TRUE)
#'
#'
#' ### visualize fit ------------------
#'
#' plot(vm_pud_only, ylim = c(-3,3), difference = TRUE)
#' lines(diff(nps_ts), col = "red")
#'
#' plot(vm_ref_series, ylim = c(-3,3), difference = TRUE)
#' lines(diff(nps_ts), col = "red")
#'
#'


visitation_model <- function(onsite_usage,
                             popularity_proxy = NULL,
                             suspected_periods = c(12,6,4,3),
                             proportion_of_variance_type = c("leave_out_first", "total"),
                             max_proportion_of_variance = 0.995,
                             log_ratio_cutoff = 0.2,
                             window_length = "auto",
                             num_trend_components = 2,
                             criterion = c("cross-correlation","MSE","rank"),
                             possible_lags = -36:36,
                             leave_off = 6,
                             estimated_change = 0,
                             order_of_polynomial_approximation = 7,
                             order_of_derivative = 1,
                             ref_series = NULL,
                             beta = "estimate",
                             constant = 0,
                             log_scale = TRUE,
                             spline = FALSE,
                             parameter_estimates = c("separate","joint"),
                             omit_trend = TRUE,
                             ...){

  # replace negative infinities with an appropriate value using robust normal approximation

  if(sum(na.omit(onsite_usage) == -Inf) > 0)
  {
    negative_infs_onsite_usage <- as.numeric(onsite_usage == -Inf)
    negative_inf_loc_onsite_usage <- which((onsite_usage == -Inf) == TRUE)
    ratio_negative_infs_onsite_usage <- mean(negative_infs_onsite_usage, na.rm = TRUE)
    if(sum(negative_infs_onsite_usage, na.rm = TRUE) > 0){
      exp_onsite_usage <- exp(onsite_usage)
      exp_Qs_onsite_usage <- quantile(exp_onsite_usage, probs=c(0.25,0.5,0.75))
      if((exp_Qs_onsite_usage)[1]==0)
      {
        stop("Too many negative infinities in onsite_usage.")
      }
      center_onsite_usage <- log(exp_Qs_onsite_usage)[2]
      IQR_onsite_usage <- log(exp_Qs_onsite_usage)[3] - log(exp_Qs_onsite_usage)[1]
      sd_onsite_usage <- IQR_onsite_usage/(qnorm(0.75)-qnorm(0.25))
      min_onsite_usage <- min(onsite_usage[-negative_inf_loc_onsite_usage], na.rm = TRUE)
      candidate_onsite_usage <- qnorm(ratio_negative_infs_onsite_usage/2, mean=center_onsite_usage, sd=sd_onsite_usage)
      replace_onsite_usage <- min(min_onsite_usage, candidate_onsite_usage, na.rm = TRUE)
      onsite_usage[negative_inf_loc_onsite_usage] <- replace_onsite_usage
    }
  }


  if(!is.null(ref_series)){
    if(sum(na.omit(ref_series) == -Inf) > 0)
    {
      negative_infs_ref_series <- as.numeric(ref_series == -Inf)
      negative_inf_loc_ref_series <- which((ref_series == -Inf) == TRUE)
      ratio_negative_infs_ref_series <- mean(negative_infs_ref_series, na.rm = TRUE)
      if(sum(negative_infs_ref_series, na.rm = TRUE) > 0){
        exp_ref_series <- exp(ref_series)
        exp_Qs_ref_series <- quantile(exp_ref_series, probs=c(0.25,0.5,0.75))
        if((exp_Qs_ref_series)[1]==0)
        {
          stop("Too many negative infinities in ref_series.")
        }
        center_ref_series <- log(exp_Qs_ref_series)[2]
        IQR_ref_series <- log(exp_Qs_ref_series)[3] - log(exp_Qs_ref_series)[1]
        sd_ref_series <- IQR_ref_series/(qnorm(0.75)-qnorm(0.25))
        min_ref_series <- min(ref_series[-negative_inf_loc_ref_series], na.rm = TRUE)
        candidate_ref_series <- qnorm(ratio_negative_infs_ref_series/2, mean=center_ref_series, sd=sd_ref_series)
        replace_ref_series <- min(min_ref_series, candidate_ref_series, na.rm = TRUE)
        ref_series[negative_inf_loc_ref_series] <- replace_ref_series
      }
    }
  }

  arguments <- c(as.list(environment()), list(...))

  do.call(check_arguments,arguments)

  # decompose onsite_usage time series ------------------------------------------------------------------
  arguments[["onsite_usage_decomposition"]] <- auto_decompose(time_series = onsite_usage,
                                                              suspected_periods = suspected_periods,
                                                              proportion_of_variance_type = proportion_of_variance_type,
                                                              max_proportion_of_variance = max_proportion_of_variance,
                                                              log_ratio_cutoff = log_ratio_cutoff,
                                                              window_length = window_length,
                                                              num_trend_components = num_trend_components)


  # decompose proxy for popularity of social media ------------------------------------------------------
  if(omit_trend == FALSE){
    arguments[["popularity_proxy_decomposition_data"]] <- do.call(decompose_proxy,arguments)
  }else{
    arguments[["popularity_proxy_decomposition_data"]] <- list(forecasts_needed = 0, lag_estimate = list(lag = 0))
  }


  # estimate beta and constant parameters ---------------------------------------------------------------
  arguments[["parameter_estimates_and_time_series_windows"]] <- do.call(estimate_parameters,arguments)


  # obtain fitted values of model ------------------------------------------------------------------------
  fitted_values <- do.call(fit_model,arguments)


  return(new_visitation_model(visitation_fit = fitted_values,
                              differenced_fit = diff(fitted_values),
                              beta = as.numeric(arguments$parameter_estimates_and_time_series_windows$beta),
                              constant = as.numeric(exp(arguments$parameter_estimates_and_time_series_windows$constant)),
                              proxy_decomposition = arguments$popularity_proxy_decomposition_data$proxy_decomposition,
                              onsite_usage_decomposition = arguments$onsite_usage_decomposition,
                              forecasts_needed = arguments$popularity_proxy_decomposition_data$forecasts_needed,
                              lag_estimate = arguments$popularity_proxy_decomposition_data$lag_estimate,
                              criterion = criterion,
                              ref_series = ref_series,
                              omit_trend = omit_trend,
                              call = match.call())
  )

}

#' @title Decompose Popularity Proxy
#' @description Decomposes the popularity proxy time series into trend and seasonality components.
#' @export
#' @param onsite_usage A vector which stores on-site usage in the log scale for a particular social media platform and recreational site.
#' @param popularity_proxy A vector which stores a time series which may be used as a proxy for the social media time series in the log scale. The length of popularity_proxy must be the same as that of onsite_usage. The default option is NULL, in which case, no proxy needs to be supplied.
#' @param suspected_periods A vector which stores the suspected periods in the descending order of importance. The default option is c(12,6,4,3), corresponding to 12, 6, 4, and 3 months if observations are monthly.
#' @param proportion_of_variance_type A character string specifying the option for choosing the maximum number of eigenvalues based on the proportion of total variance explained. If "leave_out_first" is chosen, then the contribution made by the first eigenvector is ignored; otherwise, if "total" is chosen, then the contribution made by all the eigenvectors is considered.
#' @param max_proportion_of_variance A numeric specifying the proportion of total variance explained using the method specified in proportion_of_variance_type. The default option is 0.995.
#' @param log_ratio_cutoff A numeric specifying the threshold for the deviation between the estimated period and candidate periods in suspected_periods. The default option is 0.2, which means that if the absolute log ratio between the estimated and candidate period is within 0.2 (approximately a 20 percent difference), then the estimated period is deemed equal to the candidate period.
#' @param window_length A character string or positive integer specifying the window length for the SSA estimation. If "auto" is chosen, then the algorithm automatically selects the window length by taking a multiple of 12 which does not exceed half the length of onsite_usage. The default option is "auto".
#' @param num_trend_components A positive integer specifying the number of eigenvectors to be chosen for describing the trend in SSA. The default option is 2. This is relevant only when omit_trend is FALSE.
#' @param criterion A character string specifying the criterion for estimating the lag in popularity_proxy. If "cross-correlation" is chosen, it chooses the lag that maximizes the correlation coefficient between lagged popularity_proxy and onsite_usage. If "MSE" is chosen, it does so by identifying the lagged popularity_proxy whose derivative is closest to that of onsite_usage by minimizing the mean squared error. If "rank" is chosen, it does so by firstly ranking the square errors of the derivatives and identifying the lag which would minimize the mean rank.
#' @param possible_lags A numeric vector specifying all the candidate lags for popularity_proxy. The default option is -36:36.  This is relevant only when omit_trend is FALSE.
#' @param leave_off A positive integer specifying the number of observations to be left off when estimating the lag. The default option is 6. This is relevant only when omit_trend is FALSE.
#' @param estimated_change A numeric specifying the estimated change in the visitation trend. The default option is 0, implying no change in the trend.
#' @param order_of_polynomial_approximation A numeric specifying the order of the polynomial approximation of the difference between time series used in \code{estimate_lag}. The default option is 7, the seventh-degree polynomial. This is relevant only when omit_trend is FALSE.
#' @param order_of_derivative A numeric specifying the order of derivative for the approximated difference between time_series1 and lagged time_series2. The default option is 1, the first derivative. This is relevant only when omit_trend is FALSE.
#' @param ref_series A numeric vector specifying the original visitation series in the log scale. The default option is NULL, implying that no such series is available. If such series is available, then its length must be the same as that of time_series.
#' @param beta A numeric or a character string specifying the seasonality adjustment factor. The default option is "estimate", in which case, it is estimated by using the Fisher's z-transformed lag-12 autocorrelation. Even if an actual value is supplied, if ref_series is supplied, it is overwritten by the least squares estimate.
#' @param constant A numeric specifying the constant term in the model. This constant is understood as the mean of the trend-adjusted time_series. The default option is 0, implying that the time_series well represents the actual visitation counts, which is rarely the case. If ref_series is supplied, the constant is overwritten by the least squares estimate.
#' @param log_scale A Boolean specifying whether or not the results should be returned in the log scale. The default option is TRUE, in which case, the results are returned in the log scale.
#' @param spline A Boolean specifying whether or not to use a smoothing spline for the lag estimation. This is relevant only when omit_trend is FALSE.
#' @param parameter_estimates A character string specifying how to estimate beta and constant parameters should a reference series be supplied. Both options use least squares estimates, but "separate" indicates that the differenced series should be used to estimate beta separately from the constant, while "joint" indicates to estimate both using non-differenced detrended series.
#' @param omit_trend A Boolean specifying whether or not to consider the trend component to be 0. The default option is TRUE, in which case, the trend component is 0.
#' @param onsite_usage_decomposition A "decomposition" class object containing decomposition data for the onsite usage time series (outputs from `auto_decompose`).
#' @param ... Additional arguments to be passed onto the smoothing spline (\code{smooth.spline}).
#'
#' @return
#' \item{proxy_decomposition}{A "decomposition" object representing the automatic decomposition obtained from popularity_proxy (see auto_decompose())}
#' \item{lagged_proxy_trend_and_forecasts_window}{A `ts` object storing the potentially lagged popularity proxy trend and any forecasts needed due to the lag}
#' \item{ts_trend_window}{A `ts` object storing the trend component of the onsite social media usage. This trend component is potentially truncated to match available popularity proxy data.}
#' \item{ts_seasonality_window}{A `ts` object storing the seasonality component of the onsite social media usage. This seasonality component is potentially truncated to match available popularity proxy data.}
#' \item{latest_starttime}{A `tsp` attribute of a `ts` object representing the latest of the two start times of the potentially lagged populairty proxy and the onsite social media usage.}
#' \item{endtime}{A `tsp` attribute of a `ts` object representing the time of the final onsite usage observation.}
#' \item{forecasts_needed}{An integer representing the number of forecasts of popularity_proxy needed to obtain all fitted values. Negative values indicate extra observations which may be useful for predictions.}
#' \item{lag_estimate}{A list storing both the MSE-based esitmate and Rank-based estimates for the lag.}







decompose_proxy <- function(onsite_usage,
                            popularity_proxy = NULL,
                            suspected_periods = c(12,6,4,3),
                            proportion_of_variance_type = c("leave_out_first", "total"),
                            max_proportion_of_variance = 0.995,
                            log_ratio_cutoff = 0.2,
                            window_length = "auto",
                            num_trend_components = 2,
                            criterion = c("cross-correlation","MSE","rank"),
                            possible_lags = -36:36,
                            leave_off = 6,
                            estimated_change = 0,
                            order_of_polynomial_approximation = 7,
                            order_of_derivative = 1,
                            ref_series = NULL,
                            beta = "estimate",
                            constant = 0,
                            log_scale = TRUE,
                            spline = FALSE,
                            parameter_estimates = c("separate","joint"),
                            omit_trend = TRUE,
                            onsite_usage_decomposition,
                            ...){

  proxy_decomposition <- auto_decompose(time_series = popularity_proxy,
                                        suspected_periods = suspected_periods,
                                        proportion_of_variance_type = proportion_of_variance_type,
                                        max_proportion_of_variance = max_proportion_of_variance,
                                        log_ratio_cutoff = log_ratio_cutoff,
                                        window_length = window_length,
                                        num_trend_components = num_trend_components)

  proxy_trend <- proxy_decomposition$reconstruction$Trend



  #estimate lag for trend

  lag_estimate <- estimate_lag(time_series1 = onsite_usage_decomposition$reconstruction$Trend,
                               time_series2 = proxy_trend,
                               possible_lags = possible_lags,
                               method = criterion,
                               leave_off = leave_off,
                               estimated_change = estimated_change,
                               order_of_polynomial_approximation = order_of_polynomial_approximation,
                               order_of_derivative = order_of_derivative,
                               spline = spline,
                               ...)

  proxy_trend_correction <- -lag_estimate$lag

  lagged_proxy_trend <- stats::lag(proxy_trend, k = proxy_trend_correction)

  #determine how many forecasts are needed for lagged trend proxy (some observations may be available)
  date_ts_final <- time(onsite_usage)[length(onsite_usage)]
  date_proxy_trend_final <- time(lagged_proxy_trend)[length(lagged_proxy_trend)]
  date_proxy_trend_first <- time(lagged_proxy_trend)[1]

  #if forecasts_needed is negative or 0, we will make no forecasts.
  freq <- frequency(onsite_usage)
  forecasts_needed <- as.integer(round((date_ts_final - date_proxy_trend_final) * freq ))

  trend_proxy_and_forecasts <- c()

  #trend_proxy_and_forecasts is the combined trend component and any needed forecasts.
  if(forecasts_needed > 0){
    trend_proxy_forecasts <- predict(proxy_decomposition, n_ahead = forecasts_needed, only_new = TRUE)
    trend_proxy_and_forecasts <- ts(c(lagged_proxy_trend, trend_proxy_forecasts$trend_forecast),
                                    start = date_proxy_trend_first,
                                    frequency = frequency(lagged_proxy_trend))

  } else{trend_proxy_and_forecasts <- lagged_proxy_trend}

  #A window is determined where both proxy trend and time series data is available.
  ts_trend <- onsite_usage_decomposition$reconstruction$Trend
  ts_seasonality <- onsite_usage_decomposition$reconstruction$Seasonality
  latest_starttime <- max(c(time(trend_proxy_and_forecasts)[1],
                            time(ts_trend)[1])) #latest common starttime
  endtime <- time(ts_trend)[length(ts_trend)] #earliest common endtime

  lagged_proxy_trend_and_forecasts_window <- window(trend_proxy_and_forecasts,
                                                    start = latest_starttime,
                                                    end = endtime)

  ts_trend_window <- window(ts_trend,
                            start = latest_starttime,
                            end = endtime)
  ts_seasonality_window <- window(ts_seasonality,start =latest_starttime, end = endtime)

  return(list(
    proxy_decomposition = proxy_decomposition,
    lagged_proxy_trend_and_forecasts_window = lagged_proxy_trend_and_forecasts_window,
    ts_trend_window = ts_trend_window,
    ts_seasonality_window = ts_seasonality_window,
    latest_starttime = latest_starttime,
    endtime = endtime,
    forecasts_needed = forecasts_needed,
    lag_estimate = lag_estimate
  ))

}

#' @title Estimate Parameters for Visitation Model
#' @description Estimate the two parameters (y-intercept and seasonality factor) for the visitation model.
#' @export
#' @param popularity_proxy_decomposition_data A "decomposition" class object containing decomposition data for the popularity proxy time series (outputs from `auto_decompose`).
#' @param onsite_usage A vector which stores on-site usage in the log scale for a particular social media platform and recreational site.
#' @param onsite_usage_decomposition A "decomposition" class object containing decomposition data for the onsite usage time series (outputs from `auto_decompose`).
#' @param omit_trend A Boolean specifying whether or not to consider the trend component to be 0.
#' @param ref_series A numeric vector specifying the original visitation series in the log scale.
#' @param beta A numeric or a character string specifying the seasonality adjustment factor. The default option is "estimate", in which case, it is estimated by using the Fisher's z-transformed lag 12 autocorrelation. Even if an actual value is supplied, if ref_series is supplied, it is overwritten by the least squares estimate.
#' @param constant A numeric specifying the constant term in the model. This constant is understood as the mean of the trend-adjusted time_series. If ref_series is supplied, the constant is overwritten by the least squares estimate.
#' @param parameter_estimates A character string specifying how to estimate beta and constant parameters should a reference series be supplied. Both options use least squares estimates, but "separate" indicates that the differenced series should be used to estimate beta separately from the constant, while "joint" indicates to estimate both using non-differenced detrended series.
#' @param ... Additional arguments.
#'
#'
#'
#' @return
#' \item{lagged_proxy_trend_and_forecasts_window}{A `ts` object storing the potentially lagged popularity proxy trend and any forecasts needed due to the lag}
#' \item{ts_trend_window}{A `ts` object storing the trend component of the onsite social media usage. This trend component is potentially truncated to match available popularity proxy data.}
#' \item{ts_seasonality_window}{A `ts` object storing the seasonality component of the onsite social media usage. This seasonality component is potentially truncated to match available popularity proxy data.}
#' \item{latest_starttime}{A `tsp` attribute of a `ts` object representing the latest of the two start times of the potentially lagged populairty proxy and the onsite social media usage.}
#' \item{endtime}{A `tsp` attribute of a `ts` object representing the time of the final onsite usage observation.}
#' \item{beta}{A numeric storing the estimated seasonality adjustment factor.}
#' \item{constant}{A numeric storing estimated constant term used in the model.}



estimate_parameters <- function(popularity_proxy_decomposition_data = NULL,
                                onsite_usage,
                                onsite_usage_decomposition,
                                omit_trend,
                                ref_series,
                                beta,
                                constant,
                                parameter_estimates,
                                ...
){

  # If omit_trend == TRUE, time series window variables are replaced with variables obtained
  # during the popularity proxy decomposition.
  if(omit_trend == TRUE)
  {

    ts_trend_window <- onsite_usage_decomposition$reconstruction$Trend
    ts_seasonality_window <- onsite_usage_decomposition$reconstruction$Seasonality
    latest_starttime <- time(ts_trend_window[1])
    endtime <- time(ts_trend_window[length(ts_trend_window)])
    lagged_proxy_trend_and_forecasts_window <- NULL

  }else{

    ts_trend_window <- popularity_proxy_decomposition_data$ts_trend_window
    ts_seasonality_window <- popularity_proxy_decomposition_data$ts_seasonality_window
    latest_starttime <- popularity_proxy_decomposition_data$latest_starttime
    endtime <- popularity_proxy_decomposition_data$endtime
    lagged_proxy_trend_and_forecasts_window <- popularity_proxy_decomposition_data$lagged_proxy_trend_and_forecasts_window

  }


  # when ref_series is not supplied and beta is set equal to "estimate".
  if(is.null(ref_series) && beta=="estimate")
  {
    detrended_ts <- onsite_usage-onsite_usage_decomposition$reconstruction$Trend
    lag12_acf_of_detrended_ts <- acf(detrended_ts, plot = FALSE)[[1]][[13]]
    fisher_transformed_lag12_acf_of_detrended_ts <-
      (1/2)*log((1+lag12_acf_of_detrended_ts)/(1-lag12_acf_of_detrended_ts))
    beta <- fisher_transformed_lag12_acf_of_detrended_ts
  }



  #when ref_series is supplied. The constant term and beta are estimated using least squares.
  if(!is.null(ref_series))
  {



    #if the check below is greater than zero, it returns FALSE, showing that the seasonality is present.
    seasonality_absence_check <- all.equal(sum(abs(ts_seasonality_window)), 0)

    if(omit_trend == TRUE){
      past_current_adj_trend <- rep(0,length(ref_series))
      ref_series_window <- ref_series
    }else{
      past_current_adj_trend <- c(ts_trend_window - lagged_proxy_trend_and_forecasts_window)
      ref_series_window <- window(ref_series, start = latest_starttime, end = endtime)
    }

    detrended_window <- ref_series_window - past_current_adj_trend

    #seasonality is present
    if(seasonality_absence_check != TRUE)
    {
      if(identical(parameter_estimates,"joint")){
        past_current_model <- lm(detrended_window~ts_seasonality_window)
        beta <- summary(past_current_model)$coefficients[,"Estimate"][2]
        constant <- summary(past_current_model)$coefficients[,"Estimate"][1]
      } else{
        #least squares estimates of beta and constant term are calculated.

        past_current_model <- lm(diff(detrended_window)~diff(ts_seasonality_window)-1)
        beta_estimates <- summary(past_current_model)$coefficients[,"Estimate"]
        beta <- beta_estimates[1]
        constant <- mean(detrended_window)
      }
    }
    else #seasonality is absent
    {
      #least squares estimate of the constant term is calculated. The beta estimate is set equal to zero.
      beta <- 0
      message("Since there is no detected seasonality component, the seasonality adjustment (beta) is set to zero.")
      constant <- mean(detrended_window)
    }
  }

  return(list(
    beta = beta,
    constant = constant,
    ts_trend_window = ts_trend_window,
    ts_seasonality_window = ts_seasonality_window,
    latest_starttime = latest_starttime,
    endtime = endtime,
    lagged_proxy_trend_and_forecasts_window = lagged_proxy_trend_and_forecasts_window
  ))



}

#' @title Fit Model
#' @description Fit the visitation model.
#' @export
#' @param parameter_estimates_and_time_series_windows # a list storing the outputs of `estimate_parameters`, including parameter estimates `beta` and `constant` as well as data pertaining to time series windows.
#' @param omit_trend A Boolean specifying whether or not to consider the trend component to be 0.
#' @param log_scale A Boolean specifying whether or not the results should be returned in the log scale.
#' @param ... Additional arguments
#'
#' @return
#' \item{visitation_fit}{A vector storing fitted values of visitation model.}

fit_model <- function(parameter_estimates_and_time_series_windows,
                      omit_trend,
                      log_scale,
                      ...
){

  constant <- parameter_estimates_and_time_series_windows$constant
  ts_trend_window <- parameter_estimates_and_time_series_windows$ts_trend_window
  lagged_proxy_trend_and_forecasts_window <- parameter_estimates_and_time_series_windows$lagged_proxy_trend_and_forecasts_window
  beta <- parameter_estimates_and_time_series_windows$beta
  ts_seasonality_window <- parameter_estimates_and_time_series_windows$ts_seasonality_window



  if(omit_trend == FALSE){
    log_visitation_fit <- constant + ts_trend_window - lagged_proxy_trend_and_forecasts_window + beta * ts_seasonality_window
  }
  if(omit_trend == TRUE){
    log_visitation_fit <- constant + beta * ts_seasonality_window
  }


  if(log_scale == FALSE)
  {
    visitation_fit <- exp(log_visitation_fit)

  }
  else #log_scale == TRUE
  {
    visitation_fit <- log_visitation_fit

  }

  return(visitation_fit)

}

#' @title Check Arguments
#' @description Check arguments.
#' @export
#' @param log_scale A Boolean specifying whether or not the results should be returned in the log scale.
#' @param popularity_proxy A vector which stores a time series which may be used as a proxy for the social media time series in the log scale. The length of popularity_proxy must be the same as that of onsite_usage.
#' @param onsite_usage A vector which stores on-site usage in the log scale for a particular social media platform and recreational site.
#' @param constant A numeric specifying the constant term in the model. This constant is understood as the mean of the trend-adjusted time_series. If ref_series is supplied, the constant is overwritten by the least squares estimate.
#' @param omit_trend A Boolean specifying whether or not to consider the trend component to be 0.
#' @param ref_series A numeric vector specifying the original visitation series in the log scale. If such series is available, then its length must be the same as that of time_series.
#' @param ... Additional arguments.
#'
#' @return No return value, called for extra information.

check_arguments <- function(log_scale,popularity_proxy,onsite_usage,ref_series,constant,omit_trend,...){

  if(log_scale == TRUE)
  {
    message("All the forecasts will be made in the log scale.")
  }
  if((length(popularity_proxy) != length(onsite_usage)) & !omit_trend)
  {
    stop("lengths of onsite_usage and popularity_proxy differ.")
  }
  if(constant == 0 && is.null(ref_series))
  {
    message("The additive constant for the model is assumed to be equal to zero.
            If a better constant is known, change the value in the constant argument.
            Instead, the actual series may be supplied in the ref_series argument.")
  }
  if(!is.null(ref_series))
  {
    if(length(ref_series) != length(onsite_usage))
    {
      stop("lengths of onsite_usage and ref_series differ.")
    }
  }
  if(constant != 0 && !is.null(ref_series))
  {
    message("The additive constant specified in the constant argument will be
            replaced by the least squares estimate using the series specified in
            the ref_series argument.")
  }
  if(omit_trend == TRUE && !is.null(popularity_proxy)){

    message("When omit_trend == TRUE, popularity_proxy will not be used.")

  }

}

