context("prediction of forecast")

library(VisitorCounts)
library(testthat)

#A time series representing the logged values for the popularity on social media Flickr. 
log_gtrends_popularity <- c(1.098612, 1.386294, 2.197225, 1.791759, 2.197225, 2.397895, 2.564949, 2.772589, 2.708050, 2.772589, 2.995732, 2.944439,
  3.091042, 3.135494, 3.258097, 3.367296, 3.465736, 3.663562, 3.713572, 3.806662, 3.871201, 3.891820, 3.931826, 3.970292,
  4.060443, 4.110874, 4.143135, 4.143135, 4.262680, 4.394449, 4.406719, 4.465908, 4.499810, 4.430817, 4.430817, 4.430817,
  4.406719, 4.418841, 4.406719, 4.465908, 4.499810, 4.521789, 4.574711, 4.595120, 4.553877, 4.574711, 4.584967, 4.574711,
  4.595120, 4.595120, 4.605170, 4.595120, 4.574711, 4.574711, 4.564348, 4.564348, 4.564348, 4.521789, 4.465908, 4.418841,
  4.406719, 4.454347, 4.442651, 4.442651, 4.442651, 4.418841, 4.430817, 4.382027, 4.394449, 4.369448, 4.330733, 4.304065,
  4.430817, 4.330733, 4.317488, 4.356709, 4.343805, 4.304065, 4.304065, 4.290459, 4.276666, 4.262680, 4.248495, 4.189655,
  4.174387, 4.189655, 4.143135, 4.110874, 4.094345, 4.060443, 4.060443, 4.007333, 3.988984, 3.951244, 3.951244, 3.912023,
  3.891820, 3.828641, 3.828641, 3.761200, 3.931826, 3.828641, 3.828641, 3.806662, 3.806662, 3.806662, 3.761200, 3.688879,
  3.663562, 3.688879, 3.663562, 3.663562, 3.637586, 3.637586, 3.637586, 3.583519, 3.583519, 3.555348, 3.526361, 3.433987,
  3.496508, 3.433987, 3.433987, 3.433987, 3.433987, 3.367296, 3.332205, 3.332205, 3.332205, 3.295837, 3.258097, 3.258097,
  3.044522, 3.044522, 2.995732, 2.995732, 2.995732, 2.995732, 2.890372, 2.833213, 2.890372, 2.833213, 2.833213, 2.772589,
  2.833213, 2.833213, 2.772589, 2.772589, 2.772589, 2.708050, 2.708050, 2.708050, 2.708050, 2.772589, 2.708050, 2.564949)

log_gtrends_popularity <- ts(log_gtrends_popularity,start = 2005, freq = 12)

data("park_visitation")
data("flickr_userdays")

check_trend_forecasts <- function(park_visitation, park,popularity_proxy,n_ahead){
  
  #extract pud and nps data
  pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, frequency = 12)
  pud_ts <- log(pud_ts)

  nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, frequency = 12)
  nps_ts <- log(nps_ts)


  #create visitation model
  vm <- visitation_model(pud_ts,popularity_proxy, omit_trend = FALSE, trend = "estimated", is_output_logged= TRUE, is_input_logged = TRUE)
  predict_vm <- predict(vm,n_ahead, difference = TRUE, only_new = TRUE)

  n_forecasts_needed <- n_ahead+vm$forecasts_needed #The number of forecasts we need from a prediction using the popularity proxy

  popularity_proxy_decomp <- auto_decompose(popularity_proxy) #decompose the popularity proxy into trend and seasonal components
  n_past_usable_values <- max(-vm$forecasts_needed,0) #get the number of past popularity proxy values we can use

  if(n_forecasts_needed > 0){ #if we need to make forecasts, use the popularity proxy decomp to predict these trend forecasts
    trend_forecasts <- predict(popularity_proxy_decomp,n_ahead = n_forecasts_needed)$trend_forecast #if we need to make forecasts for proxy then make them
  }else{
    trend_forecasts <- c()
    }


  n <- length(popularity_proxy_decomp$reconstruction$Trend) #get the total number of trend forecast from the decomposes popularity proxy

  past_usable_values <- numeric(n_past_usable_values) 
  extra_values <- n_past_usable_values-n_ahead #calculate endpoint for number of past_usable_values we can use

  if(n_past_usable_values > 0){ # get the past usable proxy trend values from the popularity proxy decomposition
    past_usable_values <- popularity_proxy_decomp$reconstruction$Trend[(n-n_past_usable_values+1):(n-max(c(extra_values,0)))] #get the proxy trend forecast we already have
  }


  n_needed_values <- min(n_ahead,n_forecasts_needed) #we need atleast how many we're predicting or 


  #check that the prediction function for visitation model functions same as prediction function for proxy decomposition. 

  prediction_vec <- c(predict_vm$proxy_forecasts) #proxy forecasts for a visitation model created using the provided popularity proxy
  expectation_vec <- c(past_usable_values, trend_forecasts[(length(trend_forecasts)-n_needed_values+1):length(trend_forecasts)])

  #expected is concatenation of values from popularity_proxy_decomp and forecast of trend values using popularity proxy decomp

  expect_equal(prediction_vec,expectation_vec)


}


test_that("predict.visitation_model predicts proxy trend as expected", {
  #Proxy trend forecasts should consist of some number of past trend component values and some number of new ones.

  test_parks <- c("YELL","DEVA","EVER")

  n_ahead <- 12

  for(i in seq_along(test_parks)){

  check_trend_forecasts(park_visitation,test_parks[[i]],log_gtrends_popularity,n_ahead)
  check_trend_forecasts(park_visitation,test_parks[[i]],log(flickr_userdays),n_ahead)

  }

}
)


