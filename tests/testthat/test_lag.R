context("obtaining correct lags")
library(VisitorCounts)
library(testthat)

test_that("ts_lag is recovered for a shifted x^2 time series", {

          x <- seq(-3,3,length.out = 156)
          freq = 12
          ts_lag = 3
          y1 <- ts(x^2,start = 2005, freq = freq)
          y2 <- ts(x^2,start = 2005+ts_lag/freq,freq = freq)

          #Recover ts_lag
          estimated_ts_lag <- estimate_lag(y1,y2, possible_lags = -36:36, leave_off = 0, spline = F, method = "MSE")$lag
          expect_equal(estimated_ts_lag,-ts_lag)


}


          )
