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
#'
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

  beta <- round(x$beta,3)
  constant <- round(x$constant,3)
  n_forecast <- x$n_ahead
  criterion <- x$criterion
  lag_estimate <- x$lag_estimate

  cat("Visitation model forecasts: \n \n")
  cat("Parameter Estimates: \n")
  cat("=============================== \n")
  cat(sprintf("%-20s %s","Parameter:","Estimate: \n"))
  cat(sprintf("%-20s %s","----------","--------- \n"))
  cat(sprintf("%-20s %s","Beta:",paste(beta,"\n")))
  cat(sprintf("%-20s %s","Constant:",paste(constant,"\n")))
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
#'
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
  beta <- round(x$beta,4)
  constant <- x$constant
  lag_estimate <- x$lag_estimate$lag
  criterion <- x$criterion

  cat("Call: "); print(x$call); cat("\n ")
  cat("Parameter Estimates: \n")
  cat("=============================== \n")
  cat(sprintf("%-20s %s","Parameter:","Estimate: \n"))
  cat(sprintf("%-20s %s","----------","--------- \n"))
  cat(sprintf("%-20s %s","Beta:",paste(beta,"\n")))
  cat(sprintf("%-20s %s","Constant:",paste(constant,"\n")))
  cat(sprintf("%-20s %s","Lag:",paste(lag_estimate,"\n")))
  cat(sprintf("%-20s %s","Lag Criterion:",paste(criterion,"\n")))
  cat("=============================== \n")
  invisible(x)
}



