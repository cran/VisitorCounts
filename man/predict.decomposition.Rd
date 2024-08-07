% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_predict.R
\name{predict.decomposition}
\alias{predict.decomposition}
\title{Predict Decomposition}
\usage{
\method{predict}{decomposition}(object, n_ahead, only_new = TRUE, ...)
}
\arguments{
\item{object}{An object of class "decomposition".}

\item{n_ahead}{An integer describing the number of forecasts to make.}

\item{only_new}{A Boolean describing whether or not to include past values.}

\item{...}{Additional arguments.}
}
\value{
\item{forecasts}{A vector with overall forecast values.}
\item{trend_forecasts}{A vector with trend forecast values.}
\item{seasonality_forecasts}{A vector with seasonality forecast values.}
}
\description{
Methods for generating predictions from objects of the class "decomposition".
}
\examples{

data("park_visitation")
suspected_periods <- c(12,6,4,3)
proportion_of_variance_type = "leave_out_first"
max_proportion_of_variance <- 0.995
log_ratio_cutoff <- 0.2

park <- "DEVA"

nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, freq = 12)
nps_ts <- log(nps_ts)

pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, freq = 12)
pud_ts <- log(pud_ts)

nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, freq = 12)
nps_ts <- log(nps_ts)

decomp_pud <- auto_decompose(pud_ts,
                                     suspected_periods,
                                     proportion_of_variance_type = proportion_of_variance_type,
                                     max_proportion_of_variance,
                                    log_ratio_cutoff)
n_ahead = 36
pud_predictions <- predict(decomp_pud,n_ahead = n_ahead, only_new = FALSE)



}
