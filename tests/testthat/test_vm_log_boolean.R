context("is_input_logged && is_out_logged unit tests")

library(VisitorCounts)
library(testthat)



check_inputs_outputs_logged <- function(park_visitation, park, n_ahead, vm_1_is_input_logged, vm_2_is_input_logged)
{

#partition out the training data
park_pud <- park_visitation[park_visitation$park == park,]$pud #photo user days
park_nps <- park_visitation[park_visitation$park == park,]$nps #national park service counts

park_pud.train <- ts(park_pud, start = 2005,
                            end = c(2016, 12), freq = 12)
park_nps.train <- ts(park_nps, start = 2005,
                            end = c(2016, 12), freq = 12)


# use booleans to correctly log the data
vm_1 <- NULL
vm_2 <- NULL

vm_1_ref_series <- park_nps.train
vm_1_onsite_usage <-  park_pud.train

vm_2_ref_series <- park_nps.train
vm_2_onsite_usage <-  park_pud.train

if(vm_1_is_input_logged)
{
    vm_1_ref_series <- log(vm_1_ref_series)
    vm_1_onsite_usage <- log(vm_1_onsite_usage)
}

if(vm_2_is_input_logged)
{
    vm_2_ref_series <- log(vm_2_ref_series)
    vm_2_onsite_usage <- log(vm_2_onsite_usage)
}

#create the visitation models using the boolean and training data
vm_1 <- visitation_model(ref_series = vm_1_ref_series, onsite_usage = vm_1_onsite_usage,
                                          omit_trend = TRUE, trend = "none", is_input_logged = vm_1_is_input_logged)

vm_2 <- visitation_model(ref_series = vm_2_ref_series, onsite_usage = vm_2_onsite_usage,
                                          omit_trend = TRUE, trend = "none", is_input_logged = vm_2_is_input_logged)


#make predictions for both models and set the predictions to be not-logged
vm_1_predictions <-predict(vm_1, n_ahead  = n_ahead)$forecast[1:12]
vm_2_predictions <-predict(vm_2, n_ahead  = n_ahead)$forecast[1:12]

expect_equal(vm_1_predictions, vm_2_predictions)


}



test_that("check is_logged_input and is_logged_output are working as expected", {
  #Proxy trend forecasts should consist of some number of past trend component values and some number of new ones.

   test_parks <- c("YELL","DEVA","EVER")

  n_ahead <- 12

  for(i in seq_along(test_parks)){

    check_inputs_outputs_logged(park_visitation, test_parks[[i]], n_ahead, TRUE, TRUE)

    check_inputs_outputs_logged(park_visitation, test_parks[[i]], n_ahead, FALSE, TRUE)

    check_inputs_outputs_logged(park_visitation, test_parks[[i]], n_ahead, FALSE, FALSE)

  }

}
)
