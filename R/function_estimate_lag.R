#' @title Estimate Lag Function
#' @description Uses polynomial approximation and derivatives for time series objects to estimate lag between series.
#' @import stats
#'
#' @param time_series1 A numeric vector which stores the time series of interest in the log scale.
#' @param time_series2 A numeric vector which stores the trend proxy time series in the log scale. The length of trend_proxy must be the same as that of time_series1.
#' @param possible_lags A numeric vector specifying all the candidate lags for trend_proxy. The default option is -36:36.
#' @param method A character vector specifying the method used to obtain the lag estimate. "polynomial" uses polynomial approximation, while "cross-correlation" uses cross-correlation.
#' @param leave_off A positive integer specifying the number of observations to be left off when estimating the lag.
#' @param estimated_change A numeric specifying the estimated change in the visitation trend. The default option is 0, implying no change in the trend.
#' @param order_of_polynomial_approximation A numeric specifying the order of the polynomial approximation of the difference between time series used in \code{estimate_lag}. The default option is 7, the seventh-degree polynomial.
#' @param order_of_derivative A numeric specifying the order of derivative for the approximated difference between time_series1 and lagged time_series2. The default option is 1, the first derivative.
#' @param spline A Boolean specifying whether or not to use a smoothing spline for the lag estimation.
#' @param ... Additional arguments to be passed onto the \code{smooth.spline} function, if method is "polynomial".
#'
#'
#' @return
#' \item{cc_lag}{A numeric indicating the estimated lag with the cross-correlation criterion.}
#' \item{mse_criterion}{A numeric indicating the estimated lag with the MSE criterion.}
#' \item{rank_criterion}{A numeric indicating the estimate lag with the rank criterion.}
#'
#' @export
#'
#' @examples
#' # Generate dataset with known lag and recover this lag --------------#'
#'
#' lag <- 3
#' n <- 156
#' start_year <- 2005
#' frequency <- 12
#' trend_function <- function(x) x^2
#'
#' x <- seq(-3,3, length.out = n)
#'
#' y1 <- ts(trend_function(x),start = start_year, freq = frequency)
#' y2 <- stats::lag(y1, k = lag)
#'
#'
#' # Recover lag
#' estimate_lag(y1,y2, possible_lags = -36:36,
#'              method = "rank",leave_off = 0, spline = FALSE)
#'
#'

estimate_lag <- function(time_series1,
                         time_series2,
                         possible_lags, #a vector of lags to consider, e.g., -36:36
                         method = c("cross-correlation","MSE","rank"),
                         leave_off, #the amount to leave off after the polynomial is computed
                         estimated_change = 0, #estimated change in the trend. The default option is 0.
                         order_of_polynomial_approximation = 7,
                         order_of_derivative = 1,
                         spline = FALSE,
                         ...
){

  method = match.arg(method)
  args <- c(as.list(environment()),list(...)) # generates list of arguments to this function


  lag <- switch(
    method,
    `cross-correlation` = do.call(estimate_lag_CC,args)$cc_lag,
    `MSE` =  do.call(estimate_lag_poly,args)$mse_criterion,
    `rank` = do.call(estimate_lag_poly,args)$rank_criterion,
    NA
  )

  return(list(lag = lag))
}

check_for_valid_window <- function(window_starttime,window_endtime){
  if(window_endtime <= window_starttime) stop("Invalid window for lag estimation. Try using smaller lags.")
}

estimate_lag_poly <- function(time_series1,
         time_series2,
         possible_lags, #a vector of lags to consider, eg. -36:36
         leave_off, #the amount to leave off after the polynomial is computed
         estimated_change = 0, #estimated change in the trend. The default option is 0.
         order_of_polynomial_approximation = 7,
         order_of_derivative = 1,
         spline = FALSE,
         ...
){

  #First, we obtain the start time and end time for the window to use after the polynomial approximation
  #(the observations we will consider for computing the lag).
  #to obtain largest possible window, we need to examine the time series involved:

  series1_starttime = time(time_series1)[1]
  series2_starttime = time(time_series2)[1]

  series1_endtime = time(time_series1)[length(time_series1)]
  series2_endtime = time(time_series2)[length(time_series2)]

  freq <- frequency(time_series1) #(we assume the inputs have the same frequency)

  largest_lag <- max(possible_lags)
  smallest_lag <- min(possible_lags)

  window_starttime = max(series1_starttime,series2_starttime+largest_lag/freq)+leave_off/freq
  window_endtime = min(series1_endtime,series2_endtime+smallest_lag/freq)-leave_off/freq


  check_for_valid_window(window_starttime,window_endtime)

  window_length <- round((window_endtime-window_starttime)*freq+1)
  nlags <- length(possible_lags)

  #if(window_starttime >= window_endtime) stop("No common window exists for these lags and time series. Try using smaller lags.")

  #the length of the window is 60 for leave_off = 12, possible_lags = -36:36.

  ### ------------------------------------------------------------------------------------------------
  ### For each lag, obtain difference, and for the above window, compute loss function

  lsq_value <- numeric(nlags) #least squares value
  sqdiff_values <- numeric(window_length*nlags) #a vector that stores all the squared difference values
  np_groups <- rep(possible_lags, each = window_length) #grouping used for calculating nonparametric rank-based criterion

  for(i in 1:length(possible_lags)){

    lag = possible_lags[i]

    difference_ts <- time_series1-stats::lag(time_series2, k = lag)

    derivative_approximation = approximate_ts_derivative(difference_ts,
                                                         order_of_polynomial_approximation,
                                                         order_of_derivative,
                                                         spline = spline)

    w_derivative_approximation = window(derivative_approximation, start = window_starttime,end = window_endtime)

    sqdiff = (w_derivative_approximation-estimated_change)^2

    lsq_value[i] = mean(sqdiff)

    sqdiff_values[((i-1)*window_length+1):(i*window_length)] = sqdiff


  }

  mse_criterion <- (-1)*possible_lags[which.min(lsq_value)[1]] #the one that minimizes MSE.

  np_ranked <- rank(sqdiff_values) #converting all the squared differences into ranks.
  mean_np_ranked <- tapply(np_ranked, np_groups, mean) #calculating the mean ranks for each group.
  rank_criterion <- (-1)*possible_lags[which.min(mean_np_ranked)[1]] #the one that minimizes the mean rank.

  return(list(mse_criterion=mse_criterion, rank_criterion=rank_criterion))
}



cross_correlation <- function(time_series1,time_series2,possible_lags = -20:20){

  lags <- possible_lags
  nlags <- length(lags)

  crosscor <- numeric(nlags)

  for(i in seq_along(lags)){
    ts_w <- stats::lag(time_series2,lags[i])
    X <- ts.intersect(time_series1,ts_w)
    crosscor[i] <- cor(X)[1,2]

  }

  return(list(crosscor = crosscor, lag = lags))


}

estimate_lag_CC <- function(time_series1,
                            time_series2,
                            possible_lags, #a vector of lags to consider, eg. -36:36
                            leave_off, #the amount to leave off after the polynomial is computed
                            estimated_change = 0, #estimated change in the trend. The default option is 0.
                            order_of_polynomial_approximation = 7,
                            order_of_derivative = 1,
                            spline = FALSE,
                            ...
){

  cc<- cross_correlation(time_series1,time_series2,possible_lags = possible_lags)
  cc_lag <- cc$lag[which.max(cc$crosscor)]

  return(list(cc_lag = (-1)*cc_lag)) #factor of -1 corrects for definition of lag.
}


