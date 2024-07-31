#' @title Approximate Time Series Derivative
#' @description Returns the approximate derivative of a time series.
#'
#' @param time_series A numeric vector which stores the time series of interest.
#' @param order_of_polynomial A numeric which specifies the order of the polynomial approximation.
#' @param order_of_derivative A numeric which specifies the order of the derivative.
#' @param ... Additional arguments to be passed onto the (optional) smoothing spline.
#'
#' @return
#' \item{approximation_of_derivative}{Time series object with derivative of polynomial approximation, with same time domain as original series.}
#'
#' @examples
#' ### approximate derivative of x^3+noise ----------------------
#'
#' order = 7
#' n = 100
#' sigma = 1/50
#' x = seq(-1,1,length.out = n)
#' y = x^3+rnorm(n,0,sigma)
#' y_ts <- ts(y, start = 2005, freq = 12)#'
#'
#'
#' zeroth_deriv = VisitorCounts:::approximate_ts_derivative(y_ts,order,0, spline = TRUE)
#' first_deriv = VisitorCounts:::approximate_ts_derivative(y_ts,order,1, spline = FALSE)
#' second_deriv =  VisitorCounts:::approximate_ts_derivative(y_ts,order,2)
#' third_deriv =  VisitorCounts:::approximate_ts_derivative(y_ts,order,3)
#'
#' #plots of approximation and first three derivatives ----------
#'
#' par(mfrow = c(2,2))
#' plot(zeroth_deriv, ylab = "Zeroth Derivative", main = "Zeroth Derivative")
#' plot(first_deriv, ylab = "First Derivative", main = "First Derivative")
#' plot(second_deriv, ylab = "Second Derivative", main = "Second Derivative")
#' plot(third_deriv, ylab = "Third Derivative", main = "Third Derivative")
#' @noRd



approximate_ts_derivative <- function(time_series,order_of_polynomial,order_of_derivative,spline = FALSE,...)
{


  if(spline){
    approximation_of_derivative <- predict(
      smooth.spline(time_series,...),
      deriv = order_of_derivative
    )
    return(
      ts(approximation_of_derivative$y, start = time(time_series)[1], frequency = frequency(time_series)))
  }

  n <- length(time_series)

  polynomial_model <- polynomial_approximation(time_series,order_of_polynomial)
  derivative_of_pm <- derivative_function(polynomial_model$coefficients,order_of_derivative)

  t <- (1:n)/(n)
  x <- 0

  for(i in 1:length(derivative_of_pm))
  {
    x <- x+derivative_of_pm[i]*t^(i-1)
  }

  approximation_of_derivative <- stats::ts(x,start = stats::time(time_series)[1],freq = frequency(time_series))

  return(approximation_of_derivative)
}


#' @title Derivative Function
#' @description Returns the derivative of a polynomial function.
#'
#' @param model_coefficients A numeric vector specifying the coefficients of the polynomial. The i-th polynomial coefficient can be found in the (i-1)-th entry of the vector.
#' @param number_of_derivatives A numeric which specifies the order of the derivative.
#'
#' @return
#' \item{new_coefficients}{Coefficients of derivative of user-provided polynomial.}
#'
#' @examples
#' VisitorCounts:::derivative_function(c(1,1,1,1),1) #1+x+x^2+x^3
#' @noRd


derivative_function <- function(model_coefficients,number_of_derivatives)
{

  new_coefficients <- model_coefficients

  # the length of model coefficients is one more than the order of the polynomial
  if(number_of_derivatives >= length(model_coefficients))
  {
    return(0)
  }

  #we should return the same coefficients if we take no derivatives
  if(number_of_derivatives == 0)
  {
    return(model_coefficients)
  }

  for(i in 1:number_of_derivatives)
  {
    old_coefficients <- new_coefficients
    new_coefficients <- numeric(length(old_coefficients)-1)
    for(j in 1:(length(old_coefficients)-1))
    {
      new_coefficients[j] <- old_coefficients[j+1]*j
    }
  }
  return(new_coefficients)
}





#' @title Polynomial Approximation & Derivative Function
#' @description Uses polynomial approximation and derivatives for time series objects.
#'
#' @param time_series A numeric vector which stores the time series of interest.
#' @param order_of_approximation A numeric which specifies the order of the polynomial approximation.
#'
#' @return
#' \item{lm_trend}{Trend component of time series.}
#'
#' @examples
#' data("park_visitation")
#' park <- "YELL"
#'
#' # Extract Trend Component of Time Series
#' time_series <- ts(log(park_visitation[park_visitation$park == park,]$pud),
#'                  start = 2005,
#'                  freq = 12)
#' trend <- auto_decompose(time_series)$reconstruction$Trend
#'
#' # Approximate Trend with Polynomial
#' VisitorCounts:::polynomial_approximation(trend,7)
#' @noRd

polynomial_approximation <- function(time_series,order_of_approximation)
{
  n <- length(time_series)
  t <- (1:n)/n
  lm_trend <- lm(time_series~poly(t,order_of_approximation, raw = TRUE))
  return(lm_trend)
}

#' @title Imputation
#' @description Imputation by replacing negative infinities with appropriate numbers.
#' @export
#' @param x A numeric vector (usually the log visitation counts or photo-user days).
#'
#' @return A numeric vector with the negative infinities replaced with appropriate numbers.

imputation <- function(x)
{
  if(sum(na.omit(x) == -Inf) > 0)
  {
    negative_infs_x <- as.numeric(x == -Inf)
    negative_inf_loc_x <- which((x == -Inf) == TRUE)
    ratio_negative_infs_x <- mean(negative_infs_x, na.rm = TRUE)
    if(sum(negative_infs_x, na.rm = TRUE) > 0){
      exp_x <- exp(x)
      exp_Qs_x <- quantile(exp_x, probs=c(0.25,0.5,0.75), na.rm = TRUE)
      if((exp_Qs_x)[1]==0)
      {
        stop("Too many negative infinities in x.")
      }
      center_x <- log(exp_Qs_x)[2]
      IQR_x <- log(exp_Qs_x)[3] - log(exp_Qs_x)[1]
      sd_x <- IQR_x/(qnorm(0.75)-qnorm(0.25))
      min_x <- min(x[-negative_inf_loc_x], na.rm = TRUE)
      candidate_x <- qnorm(ratio_negative_infs_x/2, mean=center_x, sd=sd_x)
      replace_x <- min(min_x, candidate_x, na.rm = TRUE)
      x[negative_inf_loc_x] <- replace_x
    }
  }
  return(x)
}

#' @title Converting Annual Counts into Monthly Counts
#' @description Convert annual counts into monthly counts using photo-user-days.
#' @export
#' @param visitation_years A numeric vector with annual visitation counts. If not available, NA should be entered.
#' @param pud A numeric vector for the monthly photo-user-days corresponding to \code{visitation_years}. As such, the length of \code{pud} needs to be exactly 12 times as long as \code{visitation_counts}.
#'
#' @return A numeric vector with estimated monthly visitation counts based on the annual counts and monthly photo-user-days.

yearsToMonths <- function(visitation_years, pud) {
  if(length(pud) != 12*length(visitation_years))
  {
    stop("The length of pud is not 12 times the length of visitation_years.")
  }
  visitation_months <- c()
  pud.prob <- c()
  for (i in 1:length(visitation_years))
  {
    if (is.na(visitation_years[i]))
    {
      visitation_months[((i-1)*12+1):(i*12)] <- rep(NA,12)
    }
    else
    {
      pud.prob <- pud[((i-1)*12+1):(i*12)]/sum(pud[((i-1)*12+1):(i*12)])
      for (j in 1:12)
      {
        visitation_months[((i-1)*12+j)] <- pud.prob[j]*visitation_years[i]
      }
    }
  }
  return(visitation_months)
}

