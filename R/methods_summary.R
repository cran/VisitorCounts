#' @title Decomposition Summary Method
#' @description S3 method for summarizing objects of the class "decomposition".
#' @export
#' @param object An object of class "decomposition".
#' @param ... Additional arguments.
#'
#' @return A "decomposition" class object.
#' @examples
#' data("park_visitation")
#'
#' park <- "YELL"
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, freq = 12)
#' nps_ts <- log(nps_ts)
#'
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, freq = 12)
#' pud_ts <- log(pud_ts)
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, freq = 12)
#' nps_ts <- log(nps_ts)#'
#'
#'
#' decomposition_pud <- auto_decompose(pud_ts)
#' decomposition_nps <- auto_decompose(nps_ts)
#' summary(decomposition_pud)
#' summary(decomposition_nps)

summary.decomposition <- function(object,...){

  neigenvectors <- dim(object$grouping)[1]

  eigenvectors <- 1:neigenvectors

  eigen_groupings <- apply(object$grouping,2,function(x){paste(eigenvectors[which(x == 1)],collapse = ", ")})


  cat("Decomposition: \n \n")

  cat(sprintf("%-20s || %s","Period or Component","Eigenvector Grouping \n"))
  cat(sprintf("%-20s || %s","===================","===================="))
  cat(sprintf("\n %-20s|| %s",colnames(object$grouping),eigen_groupings))

  cat(paste("\n \n Window Length:",object$window_length_parameter))
  cat(paste("\n Number of Observations:", length(object$reconstruction$Main_Trend)))
  cat(...)
  invisible(object)
}

#' @title visitation_forecast Summary Method
#' @description Methods for summarizing objects of the class "decomposition".
#' @export
#' @param object An object of class "decomposition".
#' @param ... Additional arguments.
#'
#' @return A "decomposition" class object.
#' @examples
#' #Example:
#'
#' data("park_visitation")
#' data("flickr_userdays")
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


summary.visitation_forecast <- function(object,...){

  # Note 
  #   The constant corresponds to the beta_0 in our model
  #   The beta parameter corresponds to the beta_1 coefficient in our model
  #   The slope (calculated only when we're estimating trend ) is the corresponding beta_2 in our model


  beta_0 <- round(object$constant,3)
  beta_1 <- round(object$beta,3)
  beta_2 <- round(object$slope,3)
  n_forecast <- object$n_ahead
  criterion <- object$criterion
  lag_estimate <- object$lag_estimate

  cat("Visitation model forecasts: \n \n")
  cat("Parameter Estimates: \n")
  cat("=============================== \n")
  cat(sprintf("%-20s %s","Parameter:","Estimate: \n"))
  cat(sprintf("%-20s %s","----------","--------- \n"))
  cat(sprintf("%-20s %s","Beta_0 (Constant):",paste(beta_0,"\n")))
  cat(sprintf("%-20s %s","Beta_1 (Seasonality):",paste(beta_1,"\n")))
  cat(sprintf("%-20s %s","Beta_2 (Trend):",paste(beta_2,"\n")))
  cat(sprintf("%-20s %s","Lag:",paste(lag_estimate,"\n")))
  cat("=============================== \n")
  cat(paste("Criterion for Lag Estimate:",criterion,"\n"))
  cat(paste("Number of Forecasts:",n_forecast,"\n"))
  invisible(object)

}

#' @title visitation_model Summary Method
#' @description Methods for summarizing objects of the class "decomposition".
#' @export
#' @param object An object of class "decomposition".
#' @param ... Additional arguments.
#'
#' @return A "decomposition" class object.
#' @examples
#' #Example:
#'
#' data("park_visitation")
#' data("flickr_userdays")
#'
#' n_ahead <- 12
#' park <- "YELL"
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, freq = 12)
#' pud_ts <- log(pud_ts)
#' trend_proxy <- log(flickr_userdays)
#'
#' vm <- visitation_model(pud_ts,trend_proxy)
#' summary(vm)


summary.visitation_model <- function(object,...){
  # Note 
  #   The constant corresponds to the beta_0 in our model
  #   The beta parameter corresponds to the beta_1 coefficient in our model
  #   The slope (calculated only when we're estimating trend ) is the corresponding beta_2 in our model

  beta_0 <- round(object$constant,3)
  beta_1 <- round(object$beta,3)
  beta_2 <- round(object$slope,3)
  lag_estimate <- object$lag_estimate$lag
  criterion <- object$criterion

  cat("Call: "); print(object$call); cat("\n ")
  cat("Parameter Estimates: \n")
  cat("=============================== \n")
  cat(sprintf("%-20s %s","Parameter:","Estimate: \n"))
  cat(sprintf("%-20s %s","----------","--------- \n"))
  cat(sprintf("%-20s %s","Beta_0 (Constant):",paste(beta_0,"\n")))
  cat(sprintf("%-20s %s","Beta_1 (Seasonality):",paste(beta_1,"\n")))
  cat(sprintf("%-20s %s","Beta_2 (Trend):",paste(beta_2,"\n")))
  cat(sprintf("%-20s %s","Lag:",paste(lag_estimate,"\n")))
  cat(sprintf("%-20s %s","Lag Criterion:",paste(criterion,"\n")))
  cat("=============================== \n")
  invisible(object)

}
