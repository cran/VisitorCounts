#' @title Decomposition Summary Method
#' @description S3 method for summarizing objects of the class "decomposition".
#' @export
#' @param x An object of class "decomposition".
#' @param ... Additional arguments.
#'
#' @return A "decomposition" class object.
#' @examples
#'  data("park_visitation")
#'
#' park <- "YELL"
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, freq = 12)
#' nps_ts <- log(nps_ts)
#'
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, freq = 12)
#' pud_ts <- log(pud_ts)
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, freq = 12)
#' nps_ts <- log(nps_ts)
#'
#'
#' decomposition_pud <- auto_decompose(pud_ts)
#' decomposition_nps <- auto_decompose(nps_ts)
#' summary(decomposition_pud)
#' summary(decomposition_nps)


print.decomposition <- function(x,...){

  neigenvectors <- dim(x$grouping)[1]

  eigenvectors <- 1:neigenvectors

  eigen_groupings <- apply(x$grouping,2,function(y){paste(eigenvectors[which(y == 1)],collapse = ", ")})


  cat("Decomposition: \n \n")

  cat(sprintf("%-20s || %s","Period or Component","Eigenvector Grouping \n"))
  cat(sprintf("%-20s || %s","===================","===================="))
  cat(sprintf("\n %-20s|| %s",colnames(x$grouping),eigen_groupings))

  cat(paste("\n \n Window Length:",x$window_length_parameter))
  cat(paste("\n Number of Observations:", length(x$reconstruction$Main_Trend)))
  cat(...)
  invisible(x)
}

#' @title visitation_forecast Summary Method
#' @description Methods for summarizing objects of the class "decomposition".
#' @export
#' @param x An object of class "decomposition".
#' @param ... Additional arguments.
#'
#' @return A "decomposition" class object.
#' @examples
#' #Example:
#'
#'data("park_visitation")
#'data("flickr_userdays")
#'
#' n_ahead <- 12
#' park <- "YELL"
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, freq = 12)
#' pud_ts <- log(pud_ts)
#' trend_proxy <- log(flickr_userdays)
#'
#' mf <- visitation_model(pud_ts,trend_proxy)
#' vf <- predict(mf,12, only_new = FALSE)
#' summary(vf)


print.visitation_forecast <- function(x,...){

  # Note 
  #   The constant corresponds to the beta_0 in our model
  #   The beta parameter corresponds to the beta_1 coefficient in our model
  #   The slope (calculated only when we're estimating trend ) is the corresponding beta_2 in our model
  beta_0 <- round(x$constant,3) 
  beta_1 <- round(x$beta,3) 
  beta_2 <- round(x$slope,3)
  n_forecast <- x$n_ahead
  criterion <- x$criterion
  lag_estimate <- x$lag_estimate

  cat("Visitation model forecasts: \n \n")
  cat("Parameter Estimates: \n")
  cat("=============================== \n")
  cat(sprintf("%-20s %s","Parameter:","Estimate: \n"))
  cat(sprintf("%-20s %s","----------","--------- \n"))
  cat(sprintf("%-20s %s","Beta_0:",paste(beta_0,"\n")))
  cat(sprintf("%-20s %s","Beta_1:",paste(beta_1,"\n")))
  cat(sprintf("%-20s %s","Beta_2:",paste(beta_2,"\n")))
  cat(sprintf("%-20s %s","Lag:",paste(lag_estimate,"\n")))
  cat("=============================== \n")
  cat(paste("Criterion for Lag Estimate:",criterion,"\n"))
  cat(paste("Number of Forecasts:",n_forecast,"\n"))
  invisible(x)
}

#' @title visitation_model Summary Method
#' @description Methods for summarizing objects of the class "decomposition".
#' @export
#' @param x An object of class "decomposition".
#' @param ... Additional arguments.
#'
#' @return A "decomposition" class object.
#' @examples
#' #Example:
#'
#'data("park_visitation")
#'data("flickr_userdays")
#'
#' n_ahead <- 12
#' park <- "YELL"
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, freq = 12)
#' pud_ts <- log(pud_ts)
#' trend_proxy <- log(flickr_userdays)
#'
#' vm <- visitation_model(pud_ts,trend_proxy)
#' summary(vm)


print.visitation_model <- function(x,...){

  # Note 
  #   The constant corresponds to the beta_0 in our model
  #   The beta parameter corresponds to the beta_1 coefficient in our model
  #   The slope (calculated only when we're estimating trend ) is the corresponding beta_2 in our model

  beta_1 <- round(x$beta,4)
  beta_0 <- x$constant
  beta_2 <- round(x$slope,4)

  lag_estimate <- x$lag_estimate$lag
  criterion <- x$criterion

  cat("Call: "); print(x$call); cat("\n ")
  cat("Parameter Estimates: \n")
  cat("=============================== \n")
  cat(sprintf("%-20s %s","Parameter:","Estimate: \n"))
  cat(sprintf("%-20s %s","----------","--------- \n"))
  cat(sprintf("%-20s %s","Beta_0:",paste(beta_0,"\n")))
  cat(sprintf("%-20s %s","Beta_1:",paste(beta_1,"\n")))
  cat(sprintf("%-20s %s","Beta_2:",paste(beta_2,"\n")))
  cat(sprintf("%-20s %s","Lag:",paste(lag_estimate,"\n")))
  cat(sprintf("%-20s %s","Lag Criterion:",paste(criterion,"\n")))
  cat("=============================== \n")
  invisible(x)
}
