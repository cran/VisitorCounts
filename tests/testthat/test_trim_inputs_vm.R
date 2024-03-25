
library(VisitorCounts)
library(testthat)
data("park_visitation")

check_trimming_correctly <-function(onsite_usage, ref_series)
{
 trimmed_data <- trim_training_data(onsite_usage = onsite_usage, ref_series = ref_series)

 #get the trimmed vectors
 trimmed_onsite_usage <- trimmed_data$onsite_usage
 trimmed_ref_series <- trimmed_data$ref_series

#get the value for how much mod 12 is leftover **should be 0**
 onsite_usage_check = length(trimmed_onsite_usage)%%12
 ref_series_check = NULL

    if(!is.null(trimmed_ref_series))
    {
        ref_series_check = length(trimmed_ref_series)%%12
        expect_equal(c(onsite_usage_check, ref_series_check, length(trimmed_ref_series)), c(0,0, length(trimmed_onsite_usage))) #check that they are mod 12 and also that ref_series and onsite_usage have same length
    }
   
    expect_equal(onsite_usage_check,0) 

}

check_trimming_errors <-function(onsite_usage, ref_series, error)
{
    expect_error(trim_training_data(onsite_usage = onsite_usage, ref_series = ref_series), error) #check that we're handling these edge cases correctly
}



test_that("check that trimming the onsite_usage and ref_series works as intended", {
  #Proxy trend forecasts should consist of some number of past trend component values and some number of new ones.

   test_parks <- c("YELL","DEVA","EVER")

  for(i in seq_along(test_parks)){

    test_parks_pud <- park_visitation[park_visitation$park == test_parks,]$pud #photo user days
    test_parks_nps<- park_visitation[park_visitation$park == test_parks,]$nps #national park service counts

    #check that we're trimming the onsite usage and ref_series to a multiple of 12 and that those two values are inputs are equal after trimmed
    check_trimming_correctly( ts(test_parks_pud, start = 2014, end = c(2016, 6), freq = 12), NULL)
    check_trimming_correctly( ts(test_parks_pud, start = 2014, end = c(2016, 6), freq = 12), ts(test_parks_nps, start = 2014, end = c(2016, 6), freq = 12)) 
    check_trimming_correctly( ts(test_parks_pud, start = 2012, end = c(2016, 12), freq = 12), ts(test_parks_nps, start = 2014, end = c(2016, 6), freq = 12)) 
    check_trimming_correctly( test_parks_pud[1:40],  NULL)      
    check_trimming_correctly( test_parks_pud[1:40],  test_parks_nps[1:40])     
    check_trimming_correctly( test_parks_pud[1:40],  test_parks_nps[1:48])    

    #check edge cases where we should be catching errors
    check_trimming_errors(NULL, NULL, "no onsite usage inputted, please put in photo-user day counts to train model." )
    check_trimming_errors(ts(test_parks_pud, start = 2015, end = c(2016, 6), freq = 12), NULL, "Must provide atleast 24 observations" )
    check_trimming_errors(test_parks_pud[1:20], NULL, "Must provide atleast 24 observations" )
    check_trimming_errors(ts(test_parks_pud, start = 2015, end = c(2016, 6), freq = 12), ts(test_parks_nps, start = 2014, end = c(2016, 6), freq = 12),
     "The overlap of ref_Series and onsite_usage must have atleast 24 observations to make a forecast" )
      check_trimming_errors(ts(test_parks_pud, start = 2015, end = c(2015, 6), freq = 12), ts(test_parks_nps, start = 2016, end = c(2016, 6), freq = 12),
     "provided ref_series and onsite usage don't overlap" )



  }

}
)
