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



