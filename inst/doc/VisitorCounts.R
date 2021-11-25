## -----------------------------------------------------------------------------
library(VisitorCounts)
data("park_visitation")
data("flickr_userdays")

## ---- fig.width = 7, fig.height = 5-------------------------------------------



yellowstone_pud <- park_visitation[park_visitation$park == "YELL",]$pud #photo user days
yellowstone_nps <- park_visitation[park_visitation$park == "YELL",]$nps #national park service counts

yellowstone_pud <- ts(yellowstone_pud, start = 2005, freq = 12)
yellowstone_nps <- ts(yellowstone_nps, start = 2005, freq = 12)

log_yellowstone_pud <- log(yellowstone_pud)
log_yellowstone_nps <- log(yellowstone_nps)

log_flickr_userdays <- log(flickr_userdays)

## ---- fig.width = 7, fig.height = 5-------------------------------------------
plot(log_yellowstone_pud, main = "Yellowstone Photo-User-Days (PUD)", ylab = "PUD")
plot(log_yellowstone_nps, main = "Yellowstone National Park Service Visitation Counts (NPS)", ylab = "NPS")
plot(log_flickr_userdays, main = "Log US Flickr user-days", ylab = "UD")

## -----------------------------------------------------------------------------
yell_visitation_model <- visitation_model(log_yellowstone_pud,
                                          log_flickr_userdays)

## -----------------------------------------------------------------------------
yell_visitation_model_nps <- visitation_model(log_yellowstone_pud,
                                              log_flickr_userdays,
                                              ref_series = log_yellowstone_nps)

## ---- fig.width = 7, fig.height = 5-------------------------------------------
true_differences <- diff(log_yellowstone_nps)
lower_bound <- min(c(true_differences,diff(yell_visitation_model$visitation_fit)))-1
upper_bound <- max(c(true_differences,diff(yell_visitation_model$visitation_fit)))

plot(yell_visitation_model, ylim = c(lower_bound, upper_bound), lwd = 2)
lines(diff(log_yellowstone_nps), col = "red")
legend("bottom",c("Model Fit","True Differences"),col = c("black","red"),lty = c(1,1))

## ---- fig.width = 7, fig.height = 5-------------------------------------------
true_differences <- diff(log_yellowstone_nps)
lower_bound <- min(c(true_differences,diff(yell_visitation_model_nps$visitation_fit)))-1
upper_bound <- max(c(true_differences,diff(yell_visitation_model_nps$visitation_fit)))

plot(yell_visitation_model_nps, ylim = c(lower_bound, upper_bound), 
     lwd = 2,
     main = "Fitted Values for Visitation Model (NPS assisted)")
lines(diff(log_yellowstone_nps), col = "red")
legend("bottom",c("Model Fit","True Differences"),col = c("black","red"),lty = c(1,1))

## -----------------------------------------------------------------------------
summary(yell_visitation_model)
summary(yell_visitation_model_nps)

## -----------------------------------------------------------------------------
yellowstone_visitation_forecasts <- predict(yell_visitation_model, n_ahead = 12)
yellowstone_visitation_forecasts_nps <- predict(yell_visitation_model_nps, n_ahead = 12)

yellowstone_visitation_forecasts_withpast <- predict(yell_visitation_model, n_ahead = 12, only_new = FALSE)

## ---- fig.width = 7, fig.height = 5-------------------------------------------
plot(yellowstone_visitation_forecasts, difference = TRUE)
plot(yellowstone_visitation_forecasts_nps, main = "Forecasts for Visitation Model (NPS Assisted)")


plot(yellowstone_visitation_forecasts_withpast, difference = TRUE)

## -----------------------------------------------------------------------------
summary(yellowstone_visitation_forecasts)
summary(yellowstone_visitation_forecasts_nps)

## -----------------------------------------------------------------------------
yell_pud_decomposition <- auto_decompose(yellowstone_pud)

## ---- fig.width = 7, fig.height = 5-------------------------------------------
plot(yell_pud_decomposition)

plot(yell_pud_decomposition, type = "period")

plot(yell_pud_decomposition, type = "classical")

## -----------------------------------------------------------------------------
summary(yell_pud_decomposition)

## ---- fig.width = 7, fig.height =5--------------------------------------------
plot(predict(yell_pud_decomposition, n_ahead = 12)$forecast, main = "Decomposition 12-ahead Forecast", ylab = "Forecast Value")

