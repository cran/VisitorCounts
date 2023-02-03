#' Popularity of Flickr, in User-Days
#'
#' A time series representing the popularity of Flickr in the United States, as measured in user-days. Here, user-days count the number of unique users posting on Flickr on a given day.
#'
#' @format A time series object with 156 observations.
#' @source Flickr. (2019). Retrieved October, 2019, from https://flickr.com/



"flickr_userdays"


#' National Park Visitation Counts and Associated Photo-User-Days Data.
#'
#' A data frame storing monthly visitation counts by National Park Service (NPS) for 20 popular US national parks and associated Flickr photo-user-days (PUD).
#' Here, photo-user-days (PUD) count the number of unique users posting a photo on Flickr on a given day from within the boundaries of a given National Park.
#'
#'
#' @format A data frame with 3276 rows and 4 variables.
#'
#' \describe{
#'   \item{date}{Date of monthly observation, in year-month-day format.}
#'   \item{park}{National Park alpha code identifying a National Park.}
#'   \item{pud}{Flickr photo-user-days (PUD). Here, PUD count the number of unique users posting a photo on flickr on a given day from within the boundaries of a given National Park.}
#'   \item{nps}{Visitation count for the corresponding park and month given by the National Park Service (NPS).}
#' }
#'
#' @source National Park Service (2018). National park service visitor use statistics. Retrieved May 10, 2018 from https://irma.nps.gov/Stats/
#' @source Flickr (2019). Retrieved October, 2019, from https://flickr.com/


"park_visitation"


#' National Forest Visitation Photo-User-Days Data.
#'
#' A data frame storing monthly visitation counts by National Forest Service (NFS) for 4 popular US national parks and associated Flickr photo-user-days (PUD).
#' Here, photo-user-days (PUD) count the number of unique users posting a photo on Flickr on a given day from within the boundaries of a given National Forest.
#'
#'
#' @format A data frame with 995 observations and 4 variables.
#'
#' \describe{
#'   \item{date}{Date of monthly observation, in year-month-day format.}
#'   \item{forest}{National Forest 3 letter identifier code, except for San Juan County which is labled as SJC.}
#'   \item{pud}{Flickr photo-user-days (PUD). Here, PUD count the number of unique users posting a photo on flickr on a given day from within the boundaries of a given National Forest.}
#'   \item{nfs}{Annual Visitation count for the corresponding forest and year given by the National Forest Service (NFS) and then distributed monthly utilizing the PUD as a proxy.}
#' }
#'
#' @source Flickr (2022). Retrieved August, 2022, from https://flickr.com/


"forest_visitation"





