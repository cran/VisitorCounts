
<!-- README.md is generated from README.Rmd. Please edit that file -->

# VisitorCounts

<!-- badges: start -->
<!-- badges: end -->

## Package Summary

The package **VisitorCounts** offers tools to forecast and estimate the
number of visitors to national parks. Our method works by using Single
Spectrum Analysis to decompose park-level photo user days(PUD), which is
the number of unique users per day that shared an image from within a
target location, into estimated trend and seasonal components. Following
appropriate adjustments, these components can be used as estimates for
the trend and seasonality components of *Δ* log(*N*<sub>*i*, *t*</sub>)(
where *N*<sub>*i*, *t*</sub> &gt; 0 denote the actual visitor counts in
month t at park i). Therefore, by using the **VisitorCounts** package
one can find the estimated percent change in a parks population by only
using social media data for that park. Our model is unique in that where
other models partially or fully utilize on-site visitor counts, ours can
achieve competitive performances on only the use of social media data.

## Example

An example of using the VisitorCounts package is forecasting the percent
change in the number of visitors to a national park for a specific
month. Lets estimate the percent change in visitors to the Yosemite
National Park for the month of August in 2022 using a model that only
relies on social media data.

First load the VisitorCounts package:

``` r
library("VisitorCounts")
```

With VisitorCounts loaded you will then need to load the appropriate
data sets:

``` r
data("park_visitation")
data("flickr_userdays")
```

The park\_visitation data set will be used to get the photo-user-day
data for Yosemite that was found on Flickr. The flickr\_userdays data
set will be used as a proxy for the popularity of Flickr and will be
used when creating a visitation\_model object.

Lets then extract the Yosemite data and put it into a time series with
these two commands:

``` r
yosemite_pud <- park_visitation[park_visitation$park == "YOSE",]$pud #photo user days

yosemite_pud <- ts(yosemite_pud, start = 2005, freq = 12)
```

Before we create a Visitation\_Model object, we should log both the
flickr\_userdays and yosemite\_pud for the best results.

``` r
log_yosemite_pud <- log(yosemite_pud)

log_flickr_userdays <- log(flickr_userdays) ##the popularity of the app. 
```

Now we can create our visitation\_model that we will be doing
predictions with:

``` r
yose_visitation_model <- visitation_model(log_yosemite_pud,
                                          log_flickr_userdays)
```

With this model we will now be able to create our forecasts:

``` r
yosemite_visitation_forecasts <- predict(yose_visitation_model, n_ahead = 60)
```

In this context n\_ahead represents the number of months we will be
forecasting. I put the value as 60 because since our time series has
data up until the end of 2017 and I want to forecast for a month in 2022
I will need to create forecasts for all the months through 2018 up until
that month in 2022. Alternatively, if n\_ahead had been set to 12 I
would only generate forecasts through the year of 2018.

Now we can plot these forecasts and look at the \_\_\_\_\_\_\_\_\_ with
the commands:

``` r
plot(yosemite_visitation_forecasts, difference = TRUE)
yosemite_visitation_forecasts$forecasts
```

Rest of findings.

## Installation

You can install the current version of VisitorCounts with:

``` r
install.packages("VisitorCounts")
```

## Main Components

you can view an in-depth explanation of the main components for this
package through the included vignette.
