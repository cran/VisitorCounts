#' @title Visitation Model
#' @description Fits a time series model that uses social media posts and popularity of the social media to model visitation to recreational sites.
#' @export
#' @param onsite_usage A vector which stores monthly on-site usage for a particular social media platform and recreational site.
#' @param popularity_proxy A vector which stores a time series which may be used as a proxy for the monthly popularity of social media over time. The length of \code{popularity_proxy} must be the same as that of \code{onsite_usage}. The default option is NULL, in which case, no proxy needs to be supplied. Note that this vector cannot have a value of 0.
#' @param suspected_periods A vector which stores the suspected periods in the descending order of importance. The default option is c(12,6,4,3), corresponding to 12, 6, 4, and 3 months if observations are monthly.
#' @param proportion_of_variance_type A character string specifying the option for choosing the maximum number of eigenvalues based on the proportion of total variance explained. If "leave_out_first" is chosen, then the contribution made by the first eigenvector is ignored; otherwise, if "total" is chosen, then the contribution made by all the eigenvectors is considered.
#' @param max_proportion_of_variance A numeric specifying the proportion of total variance explained using the method specified in \code{proportion_of_variance_type}. The default option is 0.995.
#' @param log_ratio_cutoff A numeric specifying the threshold for the deviation between the estimated period and candidate periods in suspected_periods. The default option is 0.2, which means that if the absolute log ratio between the estimated and candidate period is within 0.2 (approximately a 20 percent difference), then the estimated period is deemed equal to the candidate period.
#' @param window_length A character string or positive integer specifying the window length for the SSA estimation. If "auto" is chosen, then the algorithm automatically selects the window length by taking a multiple of 12 which does not exceed half the length of \code{onsite_usage}. The default option is "auto".
#' @param num_trend_components A positive integer specifying the number of eigenvectors to be chosen for describing the trend in SSA. The default option is 2. This is relevant only when \code{trend} is "estimated".
#' @param criterion A character string specifying the criterion for estimating the lag in \code{popularity_proxy}. If "cross-correlation" is chosen, it chooses the lag that maximizes the correlation coefficient between lagged \code{popularity_proxy} and \code{onsite_usage}. If "MSE" is chosen, it does so by identifying the lagged \code{popularity_proxy} whose derivative is closest to that of \code{onsite_usage} by minimizing the mean squared error. If "rank" is chosen, it does so by firstly ranking the square errors of the derivatives and identifying the lag which would minimize the mean rank.
#' @param possible_lags A numeric vector specifying all the candidate lags for \code{popularity_proxy}. The default option is -36:36.  This is relevant only when \code{trend} is "estimated".
#' @param leave_off A positive integer specifying the number of observations to be left off when estimating the lag. The default option is 6. This is relevant only when \code{trend} is "estimated".
#' @param estimated_change A numeric specifying the estimated change in the visitation trend. The default option is 0, implying no change in the trend.
#' @param order_of_polynomial_approximation A numeric specifying the order of the polynomial approximation of the difference between time series used in \code{estimate_lag}. The default option is 7, the seventh-degree polynomial. This is relevant only when \code{trend} is "estimated".
#' @param order_of_derivative A numeric specifying the order of derivative for the approximated difference between lagged \code{popularity_proxy} and \code{onsite_usage}. The default option is 1, the first derivative. This is relevant only when \code{trend} is "estimated".
#' @param ref_series A numeric vector specifying the original visitation series. The default option is NULL, implying that no such series is available. If such series is available, then its length must be the same as that of \code{onsite_usage}.
#' @param constant A numeric specifying the constant term (beta0) in the model. This constant is understood as the mean log adjusted monthly visitation relative to the base month. The default option is 0, implying that the (logged) \code{onsite_usage} does not require any constant shift, which is unusual. If \code{ref_series} is supplied, the constant is overwritten by the least squares estimate.
#' @param beta A numeric or a character string specifying the seasonality adjustment factor (beta1). The default option is "estimate", in which case, it is estimated by using the Fisher's z-transformed lag-12 autocorrelation. Even if an actual value is supplied, if \code{ref_series} is supplied, it is overwritten by the least squares estimate.
#' @param slope A numeric specifying the slope coefficient (beta2) in the model. This constant is applicable only when \code{trend} is set to "linear". The default option is 0, implying that the linear trend is absent.
#' @param is_input_logged A Boolean describing whether the \code{onsite_usage}, \code{ref_series}, and \code{popularity_proxy} are in the log scale. The default option is FALSE, in which case the inputs will be assumed to not be logged and will be logged before making forecasts. Setting it to TRUE will assume the inputs are logged.
#' @param spline A Boolean specifying whether or not to use a smoothing spline for the lag estimation. This is relevant only when \code{trend} is "estimated".
#' @param parameter_estimates A character string specifying how to estimate beta and constant parameters should a reference series be supplied. Both options use least squares estimates, but "separate" indicates that the differenced series should be used to estimate beta separately from the constant, while "joint" indicates to estimate both using non-differenced detrended series.
#' @param omit_trend This is obsolete and is left only for compatibility. In other words, \code{trend} will overwrite any option chosen in \code{omit_trend}. If \code{trend} is NULL, then \code{trend} is overwritten according to \code{omit_trend}. It is a Boolean specifying whether or not to consider the trend component to be 0. The default option is TRUE, in which case, the trend component is 0. If it is set to FALSE, then it is estimated using data.
#' @param trend A character string specifying how the trend is modeled. Can be any of NULL, "linear", "none", and "estimated", where "none" and "estimated" correspond to \code{omit_trend} being TRUE and FALSE, respectively. If NULL, then it follows the value specified in \code{omit_trend}.
#' @param ... Additional arguments to be passed onto the smoothing spline (\code{smooth.spline}).
#'
#' @return
#' \item{visitation_fit}{A vector storing fitted values of visitation model.}
#' \item{differenced_fit}{A vector storing differenced fitted values of visitation model. (Equal to \code{diff(visitation_fit)}.)}
#' \item{constant}{A numeric storing estimated constant term used in the model (beta0).}
#' \item{beta}{A numeric storing the estimated seasonality adjustment factor (beta1).}
#' \item{slope}{A numeric storing estimated slope coefficient term used in the model (beta2).}
#' \item{proxy_decomposition}{A "decomposition" object representing the automatic decomposition obtained from \code{popularity_proxy} (see \code{\link{auto_decompose}}).}
#' \item{time_series_decomposition}{A "decomposition" object representing the automatic decomposition obtained from \code{onsite_usage} (see \code{\link{auto_decompose}}).}
#' \item{forecasts_needed}{An integer representing the number of forecasts of \code{popularity_proxy} needed to obtain all fitted values. Negative values indicate extra observations which may be useful for predictions.}
#' \item{lag_estimate}{A list storing both the MSE-based estimate and rank-based estimates for the lag.}
#' \item{criterion}{A string; one of "cross-correlation", "MSE", or "rank", specifying the method used to select the appropriate lag.}
#' \item{ref_series}{The reference series, if one was supplied.}
#' \item{omit_trend}{Whether or not trend was considered 0 in the model. This is obsolete and is left only for compatibility.}
#' \item{trend}{The trend used in the model.}
#' \item{call}{The model call.}
#'
#' @seealso See \code{\link{predict.visitation_model}} for forecast methods, \code{\link{estimate_lag}} for details on the lag estimation, and \code{\link{auto_decompose}} for details on the automatic decomposition of time series using singular spectrum analysis (SSA). See the package \link[Rssa]{Rssa} for details regarding singular spectrum analysis.
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
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, frequency = 12)
#'
#'
#' ### fit three models ---------------
#'
#' vm_pud_linear <- visitation_model(onsite_usage = pud_ts,
#'                                   ref_series = nps_ts,
#'                                   parameter_estimates = "joint",
#'                                   trend = "linear")
#' vm_pud_only <- visitation_model(onsite_usage = pud_ts,
#'                                 popularity_proxy = flickr_userdays,
#'                                 trend = "estimated")
#' vm_ref_series <- visitation_model(onsite_usage = pud_ts,
#'                                   popularity_proxy = flickr_userdays,
#'                                   ref_series = nps_ts,
#'                                   parameter_estimates = "separate",
#'                                   possible_lags = -36:36,
#'                                   trend = "none")
#'
#'
#' ### visualize fit ------------------
#'
#' plot(vm_pud_linear, ylim = c(-3,3), difference = TRUE)
#' lines(diff(nps_ts), col = "red")
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
                             constant = 0,
                             beta = "estimate",
                             slope = 0,
                             is_input_logged = FALSE,
                             spline = FALSE,
                             parameter_estimates = c("joint", "separate"),
                             omit_trend = TRUE,
                             trend = c("linear", "none", "estimated"),
                             ...){

  # specifying a correct trend input argument. This is just for compatibility.

  proportion_of_variance_type <- match.arg(proportion_of_variance_type)
  criterion <- match.arg(criterion)
  parameter_estimates <- match.arg(parameter_estimates)
  trend <- match.arg(trend)

  if(is.null(trend))
  {
    if(omit_trend == TRUE)
    {
      trend = "none"
    }
    if(omit_trend == FALSE)
    {
      trend = "estimated"
    }
  }

  if(trend == "none")
  {
    omit_trend = TRUE
  }
  if(trend == "estimated")
  {
    omit_trend = FALSE
  }

  if(!is_input_logged)
  { #if input is not logged

    #log input only if they're not NULL

    if(!is.null(onsite_usage))
    {
      onsite_usage = log(onsite_usage)
    }

    if(!is.null(popularity_proxy))
    {
      popularity_proxy = log(popularity_proxy)
    }

    if(!is.null(ref_series))
    {
      ref_series = log(ref_series)
    }
  }

  is_input_logged <- TRUE

  # replace negative infinities with an appropriate value using robust normal approximation

  if(sum(na.omit(onsite_usage) == -Inf) > 0)
  {#handle infinites if onsite_usage is logged (log(0))
    negative_infs_ref_series <- as.numeric(ref_series == -Inf)
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
    { #handle infinites if ref series is logged (log(0))
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

  #trim the inputs so we are only getting multiple of 12
  trimmed_inputs <- trim_training_data(onsite_usage = onsite_usage, ref_series = ref_series)
#
 onsite_usage = trimmed_inputs$onsite_usage
 ref_series = trimmed_inputs$ref_series

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
  if(trend == "estimated")
  {
    arguments[["popularity_proxy_decomposition_data"]] <- do.call(decompose_proxy,arguments)
  }
  else #trend == "none"||trend == "linear"
  {
    arguments[["popularity_proxy_decomposition_data"]] <- list(forecasts_needed = 0, lag_estimate = list(lag = 0))
  }

  # estimate beta and constant parameters ---------------------------------------------------------------
  arguments[["parameter_estimates_and_time_series_windows"]] <- do.call(estimate_parameters,arguments)


  # obtain fitted values of model ------------------------------------------------------------------------
  fitted_values <- do.call(fit_model,arguments)


  return(new_visitation_model(visitation_fit = fitted_values,
                              differenced_fit = diff(fitted_values),
                              constant = as.numeric(arguments$parameter_estimates_and_time_series_windows$constant),
                              beta = as.numeric(arguments$parameter_estimates_and_time_series_windows$beta),
                              slope = as.numeric(arguments$parameter_estimates_and_time_series_windows$slope),
                              proxy_decomposition = arguments$popularity_proxy_decomposition_data$proxy_decomposition,
                              onsite_usage_decomposition = arguments$onsite_usage_decomposition,
                              forecasts_needed = arguments$popularity_proxy_decomposition_data$forecasts_needed,
                              lag_estimate = arguments$popularity_proxy_decomposition_data$lag_estimate,
                              criterion = criterion,
                              ref_series = ref_series,
                              omit_trend = omit_trend,
                              trend = trend,
                              call = match.call()
                                                    )  )

}

#' @title Decompose Popularity Proxy
#' @description Decomposes the popularity proxy time series into trend and seasonality components.
#' @export
#' @param onsite_usage A vector which stores monthly on-site usage for a particular social media platform and recreational site.
#' @param popularity_proxy A vector which stores a time series which may be used as a proxy for the monthly popularity of social media over time. The length of \code{popularity_proxy} must be the same as that of \code{onsite_usage}. The default option is NULL, in which case, no proxy needs to be supplied. Note that this vector cannot have a value of 0.
#' @param suspected_periods A vector which stores the suspected periods in the descending order of importance. The default option is c(12,6,4,3), corresponding to 12, 6, 4, and 3 months if observations are monthly.
#' @param proportion_of_variance_type A character string specifying the option for choosing the maximum number of eigenvalues based on the proportion of total variance explained. If "leave_out_first" is chosen, then the contribution made by the first eigenvector is ignored; otherwise, if "total" is chosen, then the contribution made by all the eigenvectors is considered.
#' @param max_proportion_of_variance A numeric specifying the proportion of total variance explained using the method specified in \code{proportion_of_variance_type}. The default option is 0.995.
#' @param log_ratio_cutoff A numeric specifying the threshold for the deviation between the estimated period and candidate periods in suspected_periods. The default option is 0.2, which means that if the absolute log ratio between the estimated and candidate period is within 0.2 (approximately a 20 percent difference), then the estimated period is deemed equal to the candidate period.
#' @param window_length A character string or positive integer specifying the window length for the SSA estimation. If "auto" is chosen, then the algorithm automatically selects the window length by taking a multiple of 12 which does not exceed half the length of \code{onsite_usage}. The default option is "auto".
#' @param num_trend_components A positive integer specifying the number of eigenvectors to be chosen for describing the trend in SSA. The default option is 2. This is relevant only when \code{trend} is "estimated".
#' @param criterion A character string specifying the criterion for estimating the lag in \code{popularity_proxy}. If "cross-correlation" is chosen, it chooses the lag that maximizes the correlation coefficient between lagged \code{popularity_proxy} and \code{onsite_usage}. If "MSE" is chosen, it does so by identifying the lagged \code{popularity_proxy} whose derivative is closest to that of \code{onsite_usage} by minimizing the mean squared error. If "rank" is chosen, it does so by firstly ranking the square errors of the derivatives and identifying the lag which would minimize the mean rank.
#' @param possible_lags A numeric vector specifying all the candidate lags for \code{popularity_proxy}. The default option is -36:36.  This is relevant only when \code{trend} is "estimated".
#' @param leave_off A positive integer specifying the number of observations to be left off when estimating the lag. The default option is 6. This is relevant only when \code{trend} is "estimated".
#' @param estimated_change A numeric specifying the estimated change in the visitation trend. The default option is 0, implying no change in the trend.
#' @param order_of_polynomial_approximation A numeric specifying the order of the polynomial approximation of the difference between time series used in \code{estimate_lag}. The default option is 7, the seventh-degree polynomial. This is relevant only when \code{trend} is "estimated".
#' @param order_of_derivative A numeric specifying the order of derivative for the approximated difference between lagged \code{popularity_proxy} and \code{onsite_usage}. The default option is 1, the first derivative. This is relevant only when \code{trend} is "estimated".
#' @param ref_series A numeric vector specifying the original visitation series. The default option is NULL, implying that no such series is available. If such series is available, then its length must be the same as that of \code{onsite_usage}.
#' @param constant A numeric specifying the constant term (beta0) in the model. This constant is understood as the mean log adjusted monthly visitation relative to the base month. The default option is 0, implying that the (logged) \code{onsite_usage} does not require any constant shift, which is unusual. If \code{ref_series} is supplied, the constant is overwritten by the least squares estimate.
#' @param beta A numeric or a character string specifying the seasonality adjustment factor (beta1). The default option is "estimate", in which case, it is estimated by using the Fisher's z-transformed lag-12 autocorrelation. Even if an actual value is supplied, if \code{ref_series} is supplied, it is overwritten by the least squares estimate.
#' @param slope A numeric specifying the slope coefficient (beta2) in the model. This constant is applicable only when \code{trend} is set to "linear". The default option is 0, implying that the linear trend is absent.
#' @param is_input_logged A Boolean describing whether the \code{onsite_usage}, \code{ref_series}, and \code{popularity_proxy} are in the log scale. The default option is FALSE, in which case the inputs will be assumed to not be logged and will be logged before making forecasts. Setting it to TRUE will assume the inputs are logged.
#' @param spline A Boolean specifying whether or not to use a smoothing spline for the lag estimation. This is relevant only when \code{trend} is "estimated".
#' @param parameter_estimates A character string specifying how to estimate beta and constant parameters should a reference series be supplied. Both options use least squares estimates, but "separate" indicates that the differenced series should be used to estimate beta separately from the constant, while "joint" indicates to estimate both using non-differenced detrended series.
#' @param omit_trend This is obsolete and is left only for compatibility. In other words, \code{trend} will overwrite any option chosen in \code{omit_trend}. If \code{trend} is NULL, then \code{trend} is overwritten according to \code{omit_trend}. It is a Boolean specifying whether or not to consider the trend component to be 0. The default option is TRUE, in which case, the trend component is 0. If it is set to FALSE, then it is estimated using data.
#' @param trend A character string specifying how the trend is modeled. Can be any of NULL, "linear", "none", and "estimated", where "none" and "estimated" correspond to \code{omit_trend} being TRUE and FALSE, respectively. If NULL, then it follows the value specified in \code{omit_trend}.
#' @param onsite_usage_decomposition A "decomposition" class object containing decomposition data for the onsite usage time series (outputs from `auto_decompose`).
#' @param ... Additional arguments to be passed onto the smoothing spline (\code{smooth.spline}).
#'
#' @return
#' \item{proxy_decomposition}{A "decomposition" object representing the automatic decomposition obtained from popularity_proxy (see \code{\link{auto_decompose}}).}
#' \item{lagged_proxy_trend_and_forecasts_window}{A `ts` object storing the potentially lagged popularity proxy trend and any forecasts needed due to the lag.}
#' \item{ts_trend_window}{A `ts` object storing the trend component of the onsite social media usage. This trend component is potentially truncated to match available popularity proxy data.}
#' \item{ts_seasonality_window}{A `ts` object storing the seasonality component of the onsite social media usage. This seasonality component is potentially truncated to match available popularity proxy data.}
#' \item{latest_starttime}{A `tsp` attribute of a `ts` object representing the latest of the two start times of the potentially lagged populairty proxy and the onsite social media usage.}
#' \item{endtime}{A `tsp` attribute of a `ts` object representing the time of the final onsite usage observation.}
#' \item{forecasts_needed}{An integer representing the number of forecasts of popularity_proxy needed to obtain all fitted values. Negative values indicate extra observations which may be useful for predictions.}
#' \item{lag_estimate}{A list storing both the MSE-based esitmate and rank-based estimates for the lag.}


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
                            constant = 0,
                            beta = "estimate",
                            slope = 0,
                            is_input_logged = FALSE,
                            spline = FALSE,
                            parameter_estimates = c("separate", "joint"),
                            omit_trend = TRUE,
                            trend = c("linear", "none", "estimated"),
                            onsite_usage_decomposition,
                            ...){

  # specifying a correct trend input argument. This is just for compatibility.

  if(is.null(trend))
  {
    if(omit_trend == TRUE)
    {
      trend = "none"
    }
    if(omit_trend == FALSE)
    {
      trend = "estimated"
    }
  }

  if(trend == "none")
  {
    omit_trend = TRUE
  }
  if(trend == "estimated")
  {
    omit_trend = FALSE
  }

  if(!is_input_logged)
  { #if input is not logged

    #log input only if they're not NULL

    if(!is.null(onsite_usage))
    {
      onsite_usage = log(onsite_usage)
    }

    if(!is.null(popularity_proxy))
    {
      popularity_proxy = log(popularity_proxy)
    }

    if(!is.null(ref_series))
    {
      ref_series = log(ref_series)
    }
  }

  is_input_logged <- TRUE

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
#' @param popularity_proxy_decomposition_data A "decomposition" class object containing decomposition data for the popularity proxy time series (outputs from \code{\link{auto_decompose}}).
#' @param onsite_usage A vector which stores monthly onsite usage for a particular social media platform and recreational site.
#' @param onsite_usage_decomposition A "decomposition" class object containing decomposition data for the monthly onsite usage time series (outputs from \code{\link{auto_decompose}}).
#' @param omit_trend This is obsolete and is left only for compatibility. In other words, \code{trend} will overwrite any option chosen in \code{omit_trend}. If \code{trend} is NULL, then \code{trend} is overwritten according to \code{omit_trend}. It is a Boolean specifying whether or not to consider the trend component to be 0. The default option is TRUE, in which case, the trend component is 0. If it is set to FALSE, then it is estimated using data.
#' @param trend A character string specifying how the trend is modeled. Can be any of NULL, "linear", "none", and "estimated", where "none" and "estimated" correspond to \code{omit_trend} being TRUE and FALSE, respectively. If NULL, then it follows the value specified in \code{omit_trend}.
#' @param ref_series A numeric vector specifying the original visitation series. The default option is NULL, implying that no such series is available. If such series is available, then its length must be the same as that of \code{onsite_usage}.
#' @param constant A numeric specifying the constant term (beta0) in the model. This constant is understood as the mean log adjusted monthly visitation relative to the base month. The default option is 0, implying that the (logged) \code{onsite_usage} does not require any constant shift, which is unusual. If \code{ref_series} is supplied, the constant is overwritten by the least squares estimate.
#' @param beta A numeric or a character string specifying the seasonality adjustment factor (beta1). The default option is "estimate", in which case, it is estimated by using the Fisher's z-transformed lag-12 autocorrelation. Even if an actual value is supplied, if \code{ref_series} is supplied, it is overwritten by the least squares estimate.
#' @param slope A numeric specifying the slope coefficient (beta2) in the model. This constant is applicable only when \code{trend} is set to "linear". The default option is 0, implying that the linear trend is absent.
#' @param parameter_estimates A character string specifying how to estimate beta and constant parameters should a reference series be supplied. Both options use least squares estimates, but "separate" indicates that the differenced series should be used to estimate beta separately from the constant, while "joint" indicates to estimate both using non-differenced detrended series.
#' @param is_input_logged A boolean specifying if the input is logged or not
#' @param ... Additional arguments.
#'
#'
#'
#' @return
#' \item{lagged_proxy_trend_and_forecasts_window}{A `ts` object storing the potentially lagged popularity proxy trend and any forecasts needed due to the lag.}
#' \item{ts_trend_window}{A `ts` object storing the trend component of the onsite social media usage. This trend component is potentially truncated to match available popularity proxy data.}
#' \item{ts_seasonality_window}{A `ts` object storing the seasonality component of the onsite social media usage. This seasonality component is potentially truncated to match available popularity proxy data.}
#' \item{latest_starttime}{A `tsp` attribute of a `ts` object representing the latest of the two start times of the potentially lagged populairty proxy and the onsite social media usage.}
#' \item{endtime}{A `tsp` attribute of a `ts` object representing the time of the final onsite usage observation.}
#' \item{beta}{A numeric storing the estimated seasonality adjustment factor.}
#' \item{constant}{A numeric storing estimated constant term used in the model.}
#' \item{slope}{A numeric storing the estimated slope term used in the model. Applicable when the trend parameter is "linear". Otherwise, NULL is returned.}



estimate_parameters <- function(popularity_proxy_decomposition_data = NULL,
                                onsite_usage,
                                onsite_usage_decomposition,
                                omit_trend,
                                trend,
                                ref_series,
                                constant,
                                beta,
                                slope,
                                parameter_estimates,
                                is_input_logged,
                                ...
){

  # specifying a correct trend input argument. This is just for compatibility.

  if(is.null(trend))
  {
    if(omit_trend == TRUE)
    {
      trend = "none"
    }
    if(omit_trend == FALSE)
    {
      trend = "estimated"
    }
  }

  if(trend == "none")
  {
    omit_trend = TRUE
  }
  if(trend == "estimated")
  {
    omit_trend = FALSE
  }

  if(!is_input_logged)
  { #if input is not logged

    #log input only if they're not NULL

    if(!is.null(onsite_usage))
    {
      onsite_usage = log(onsite_usage)
    }

    if(!is.null(popularity_proxy))
    {
      popularity_proxy = log(popularity_proxy)
    }

    if(!is.null(ref_series))
    {
      ref_series = log(ref_series)
    }
  }

  is_input_logged <- TRUE

  # replace negative infinities with an appropriate value using robust normal approximation

  if(sum(na.omit(onsite_usage) == -Inf) > 0)
  {
    warning("Negative infinities in onsite_usage will be replaced with an estimated value.")
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
      warning("Negative infinities in ref_series will be replaced with an estimated value.")
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

  # If trend == "none", time series window variables are replaced with variables obtained
  # during the popularity proxy decomposition.
  if(trend == "none"||trend=="linear")
  {
    ts_trend_window <- onsite_usage_decomposition$reconstruction$Trend
    ts_seasonality_window <- onsite_usage_decomposition$reconstruction$Seasonality
    latest_starttime <- time(ts_trend_window[1])
    endtime <- time(ts_trend_window[length(ts_trend_window)])
    lagged_proxy_trend_and_forecasts_window <- NULL
  }
  else if(trend == "estimated")
  {
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

    if(trend == "none"||trend == "linear")
    {
      past_current_adj_trend <- rep(0,length(ref_series))
      ref_series_window <- ref_series
    }
    else if(trend == "estimated")
    {
      past_current_adj_trend <- c(ts_trend_window - lagged_proxy_trend_and_forecasts_window)
      ref_series_window <- window(ref_series, start = latest_starttime, end = endtime)
    }

    detrended_window <- ref_series_window - past_current_adj_trend
    month <- 1:length(detrended_window)

    #seasonality is present
    if(seasonality_absence_check != TRUE)
    {
      if(identical(parameter_estimates,"joint")){
        if(trend == "none"||trend == "estimated")
        {
          past_current_model <- lm(detrended_window~ts_seasonality_window)
          beta <- summary(past_current_model)$coefficients[,"Estimate"][2]
          constant <- summary(past_current_model)$coefficients[,"Estimate"][1]
          slope <- 0
        }
        else #trend == "linear"
        {
          past_current_model <- lm(detrended_window~ts_seasonality_window+month)
          slope <- summary(past_current_model)$coefficients[,"Estimate"][3]
          beta <- summary(past_current_model)$coefficients[,"Estimate"][2]
          constant <- summary(past_current_model)$coefficients[,"Estimate"][1]
        }
      } else{
        #least squares estimates of beta and constant term are calculated.
        if(trend == "none"||trend=="estimated")
        {
          past_current_model <- lm(diff(detrended_window)~diff(ts_seasonality_window)-1)
          beta_estimates <- summary(past_current_model)$coefficients[,"Estimate"]
          beta <- beta_estimates[1]
          constant <- constant
          slope <- 0
        }
        else #trend == "linear"
        {
          past_current_model <- lm(diff(detrended_window)~diff(ts_seasonality_window))
          beta_estimates <- summary(past_current_model)$coefficients[,"Estimate"]
          slope <- beta_estimates[1]
          beta <- beta_estimates[2]
          constant <- constant
        }
      }
    }

    else #seasonality is absent
    {
      if(trend == "none"||trend == "estimated")
      {
        #least squares estimate of the constant term is calculated. The beta estimate is set equal to zero.
        beta <- 0
        message("Since there is no detected seasonality component, the seasonality adjustment (beta) is set to zero.")
        constant <- mean(detrended_window)
        slope <- 0
      }
      else #trend == "linear"
      {
        beta <- 0
        past_current_model <- lm(detrended_window~month)
        beta_estimates <- summary(past_current_model)$coefficients[,"Estimate"]
        slope <- beta_estimates[2]
        constant <- beta_estimates[1]
      }
    }
  }

  return(list(
    constant = constant,
    beta = beta,
    slope = slope,
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
#' @param parameter_estimates_and_time_series_windows # a list storing the outputs of \code{\link{estimate_parameters}}, including parameter estimates `constant`, `beta`, and `slope`, as well as data pertaining to time series windows.
#' @param omit_trend This is obsolete and is left only for compatibility. In other words, \code{trend} will overwrite any option chosen in \code{omit_trend}. If \code{trend} is NULL, then \code{trend} is overwritten according to \code{omit_trend}. It is a Boolean specifying whether or not to consider the trend component to be 0. The default option is TRUE, in which case, the trend component is 0. If it is set to FALSE, then it is estimated using data.
#' @param trend A character string specifying how the trend is modeled. Can be any of NULL, "linear", "none", and "estimated", where "none" and "estimated" correspond to \code{omit_trend} being TRUE and FALSE, respectively. If NULL, then it follows the value specified in \code{omit_trend}.
#' @param ... Additional arguments
#' @param  is_input_logged a Boolean specifying if the input is logged or not.
#' @return
#' \item{visitation_fit}{A vector storing fitted values of visitation model.}

fit_model <- function(parameter_estimates_and_time_series_windows,
                      omit_trend,
                      trend,
                      is_input_logged,
                      ...
){

  # specifying a correct trend input argument. This is just for compatibility.
  if(is.null(trend))
  {
    if(omit_trend == TRUE)
    {
      trend = "none"
    }
    if(omit_trend == FALSE)
    {
      trend = "estimated"
    }
  }

  if(trend == "none")
  {
    omit_trend = TRUE
  }
  if(trend == "estimated")
  {
    omit_trend = FALSE
  }

  if(!is_input_logged)
  { #if input is not logged

    #log input only if they're not NULL

    if(!is.null(onsite_usage))
    {
      onsite_usage = log(onsite_usage)
    }

    if(!is.null(popularity_proxy))
    {
      popularity_proxy = log(popularity_proxy)
    }

    if(!is.null(ref_series))
    {
      ref_series = log(ref_series)
    }
  }

  is_input_logged <- TRUE

  constant <- parameter_estimates_and_time_series_windows$constant
  ts_trend_window <- parameter_estimates_and_time_series_windows$ts_trend_window
  lagged_proxy_trend_and_forecasts_window <- parameter_estimates_and_time_series_windows$lagged_proxy_trend_and_forecasts_window
  beta <- parameter_estimates_and_time_series_windows$beta
  ts_seasonality_window <- parameter_estimates_and_time_series_windows$ts_seasonality_window
  slope <- parameter_estimates_and_time_series_windows$slope
  month <- 1:length(c(ts_trend_window))

  if(trend == "estimate")
  {
    log_visitation_fit <- constant + ts_trend_window - lagged_proxy_trend_and_forecasts_window + beta * ts_seasonality_window
  }
  else if(trend == "none")
  {
    log_visitation_fit <- constant + beta * ts_seasonality_window
  }
  else #trend == "linear"
  {
    log_visitation_fit <- constant + beta * ts_seasonality_window + slope * month
  }


  return(log_visitation_fit)

}


#' @title Check Arguments
#' @description Check arguments.
#' @export
#' @param popularity_proxy A vector which stores a time series which may be used as a proxy for the monthly popularity of social media over time. The length of \code{popularity_proxy} must be the same as that of \code{onsite_usage}. The default option is NULL, in which case, no proxy needs to be supplied. Note that this vector cannot have a value of 0.
#' @param onsite_usage A vector which stores monthly on-site usage for a particular social media platform and recreational site.
#' @param constant A numeric specifying the constant term (beta0) in the model. This constant is understood as the mean log adjusted monthly visitation relative to the base month. The default option is 0, implying that the (logged) \code{onsite_usage} does not require any constant shift, which is unusual. If \code{ref_series} is supplied, the constant is overwritten by the least squares estimate.
#' @param omit_trend This is obsolete and is left only for compatibility. In other words, \code{trend} will overwrite any option chosen in \code{omit_trend}. If \code{trend} is NULL, then \code{trend} is overwritten according to \code{omit_trend}. It is a Boolean specifying whether or not to consider the trend component to be 0. The default option is TRUE, in which case, the trend component is 0. If it is set to FALSE, then it is estimated using data.
#' @param trend A character string specifying how the trend is modeled. Can be any of NULL, "linear", "none", and "estimated", where "none" and "estimated" correspond to \code{omit_trend} being TRUE and FALSE, respectively. If NULL, then it follows the value specified in \code{omit_trend}.
#' @param ref_series A numeric vector specifying the original visitation series. The default option is NULL, implying that no such series is available. If such series is available, then its length must be the same as that of \code{onsite_usage}.
#' @param is_input_logged A boolean specifying if the input is logged or not
#' @param ... Additional arguments.
#'
#' @return No return value, called for extra information.

check_arguments <- function(popularity_proxy, onsite_usage, constant, omit_trend, trend, ref_series, is_input_logged, ...){

  # specifying a correct trend input argument. This is just for compatibility.

  if(is.null(trend))
  {
    if(omit_trend == TRUE)
    {
      trend = "none"
    }
    if(omit_trend == FALSE)
    {
      trend = "estimated"
    }
  }

  if(trend == "none")
  {
    omit_trend = TRUE
  }
  if(trend == "estimated")
  {
    omit_trend = FALSE
  }

  if(!is_input_logged)
  { #if input is not logged

    #log input only if they're not NULL

    if(!is.null(onsite_usage))
    {
      onsite_usage = log(onsite_usage)
    }

    if(!is.null(popularity_proxy))
    {
      popularity_proxy = log(popularity_proxy)
    }

    if(!is.null(ref_series))
    {
      ref_series = log(ref_series)
    }
  }

  is_input_logged <- TRUE

  # replace negative infinities with an appropriate value using robust normal approximation

  if(sum(na.omit(onsite_usage) == -Inf) > 0)
  { #if onsite usage has been logged, handle the negative infinities (log(0))
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
    { #if ref series has been logged, handle the negative infinities (log(0))
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

  if((length(popularity_proxy) != length(onsite_usage)) & trend == "estimated")
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
    { # check here is the lengths differ
      stop("lengths of onsite_usage and ref_series differ.")
    }
  }
  if(constant != 0 && !is.null(ref_series))
  {
    message("The additive constant specified in the constant argument will be
            replaced by the least squares estimate using the series specified in
            the ref_series argument.")
  }
  if(trend != "estimated" && !is.null(popularity_proxy)){

    message("When no or linear trend is assumed, popularity_proxy will not be used.")

  }


}


#' @title trim training data
#' @description Makes sure that the provided onsite_usage and ref_series have at least 12 counts and overlap.
#' @export
#' @param onsite_usage A vector which stores monthly on-site usage for a particular social media platform and recreational site.
#' @param ref_series A numeric vector specifying the original visitation series. The default option is NULL, implying that no such series is available. If such series is available, then its length must be the same as that of \code{onsite_usage}.
#'
#'@return a list of onsite_usage and ref_series that has been trimmed and modified to share same window of time.
#'


trim_training_data <-function(onsite_usage = NULL, ref_series = NULL)
{

  if(is.null(onsite_usage))
  {
    stop("no onsite usage inputted, please put in photo-user day counts to train model.")
  }

  has_ref_series = !is.null(ref_series)

  min_date <- NULL

  if( is.ts(onsite_usage))
  { # handle case when onsite usage is a time series object

   if(has_ref_series && is.ts(ref_series))
   {# if ref_series is present and is a ts object get window that both onsite usage and ref series share.
     max_date = min(time(onsite_usage)[length(onsite_usage)], time(ref_series)[length(ref_series)])
     min_date = max(time(onsite_usage)[1], time(ref_series)[1])

     if(max_date < min_date)
     {
       stop("provided ref_series and onsite usage don't overlap")
     }
     #constrict onsite usage and ref_series to window they share.
     onsite_usage = window(onsite_usage, min_date, max_date)
     ref_series = window(ref_series, min_date, max_date)

   }

    #trim the onsite usage and ref_series to a multiple of 12
    trimmed_end <- (length(onsite_usage) - (length(onsite_usage)%%12))
    trimmed_date <- time(onsite_usage)[trimmed_end] # get the trimmed end time
    onsite_usage <- window(onsite_usage, time(onsite_usage)[1], trimmed_date)

    if(has_ref_series && is.ts(ref_series))
    {
       ref_series <- window(ref_series, time(ref_series)[1], trimmed_date)
    }
    else if(has_ref_series){
      ref_series = ref_series[1:trimmed_end]
    }

  }
  else{

    trimmed_end <- (length(onsite_usage) - (length(onsite_usage)%%12))
    onsite_usage = onsite_usage[1:trimmed_end]

    if(has_ref_series)
    { # if ref_series exists
      ref_series = ref_series[1:trimmed_end]
    }

  }

  #edge case that user in less than 12 counts, or the overlap of onsite usage and ref_series is less than 12
  if(length(onsite_usage) <24)
  {
    if(has_ref_series)
    {
      stop("The overlap of ref_Series and onsite_usage must have atleast 24 observations to make a forecast")
    }
    else
    {
      stop("Must provide atleast 24 observations")

    }

  }

  return(list( onsite_usage = onsite_usage, ref_series = ref_series ))

}


