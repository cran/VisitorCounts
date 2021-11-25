#' @title Automatic Decomposition Function
#' @description Automatically decomposes a time series using singular spectrum analysis. See package \link[Rssa]{Rssa} for details on singular spectrum analysis.
#' @importFrom Rssa ssa parestimate reconstruct
#' @export
#' @param time_series A vector which stores the time series of interest in the log scale.
#' @param suspected_periods A vector which stores the suspected periods in the descending order of importance. The default option is c(12,6,4,3), corresponding to 12, 6, 4, and 3 months.
#' @param proportion_of_variance_type A character string specifying the option for choosing the maximum number of eigenvalues based on the proportion of total variance explained. If "leave_out_first" is chosen, then the contribution made by the first eigenvector is ignored; otherwise, if "total" is chosen, then the contribution made by all the eigenvectors is considered.
#' @param max_proportion_of_variance A numeric specifying the proportion of total variance explained using the method specified in proportion_of_variance_type. The default option is 0.995.
#' @param log_ratio_cutoff A numeric specifying the threshold for the deviation between the estimated period and candidate periods in suspected_periods. The default option is 0.2, which means that, if the absolute log ratio between the estimated and candidate period is within 0.2 (approximately a 20\% difference), then the estimated period is deemed equal to the candidate period.
#' @param window_length A character string or positive integer specifying the window length for the SSA estimation. If "auto" is chosen, then the algorithm automatically selects the window length by taking a multiple of 12 which does not exceed half the length of time_series. The default option is "auto".
#' @param num_trend_components A positive integer specifying the number of eigenvectors to be chosen for describing the trend in SSA. The default option is 2.
#'
#' @return
#' \item{reconstruction}{A list containing important information about the reconstructed time series. In particular, it contains the reconstructed main trend component, overall trend component, seasonal component for each period specified in suspected_periods, and overall seasonal component.}
#' \item{grouping}{A matrix containing information about the locations of the eigenvalue groups for each period in suspected_periods and trend component. The locations are indicated by '1'.}
#' \item{window_length}{A numeric indicating the window length.}
#' \item{ts_ssa}{An ssa object storing the singular spectrum analysis decomposition.}
#'
#' @examples
#' data("park_visitation")
#'
#' ### Decompose national parks service visitor counts and flickr photo user-days
#'
#' # parameters ---------------------------------------------
#' suspected_periods <- c(12,6,4,3)
#' proportion_of_variance_type = "leave_out_first"
#' max_proportion_of_variance <- 0.995
#' log_ratio_cutoff <- 0.2
#'
#' # load data ----------------------------------------------
#'
#' park <- "YELL" #for Yellowstone National Park
#'
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, freq = 12)
#' nps_ts <- log(nps_ts)
#'
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, freq = 12)
#' pud_ts <- log(pud_ts)
#'
#' # decompose time series and plot decompositions -----------
#' decomp_pud <- auto_decompose(pud_ts,
#'                                      suspected_periods,
#'                                      proportion_of_variance_type = proportion_of_variance_type,
#'                                      max_proportion_of_variance,
#'                                      log_ratio_cutoff)
#' plot(decomp_pud)
#'
#' decomp_nps <- auto_decompose(nps_ts,suspected_periods,
#'                                        proportion_of_variance_type = proportion_of_variance_type,
#'                                      max_proportion_of_variance,log_ratio_cutoff)
#'
#' plot(decomp_nps)
#'




#############################################################
# Takes Time Series as Input
# Decomposes Time Series using SSA
# ########################################################





auto_decompose <- function(time_series,
                                   suspected_periods = c(12, 6, 4, 3),
                                   proportion_of_variance_type = c("leave_out_first","total"),
                                   max_proportion_of_variance = 0.995,
                                   log_ratio_cutoff = 0.2,
                                   window_length = "auto",
                                   num_trend_components = 2){

  n <- length(time_series)

  #Assign window length to be the greatest multiple of the largest period no more than n/2

  if(window_length == "auto"){
    window_length = (n/2) %/% max(suspected_periods) * max(suspected_periods)
  }

  proportion_of_variance_type = match.arg(proportion_of_variance_type)


  #SSA Decomposition ----------------------------------------------------------------------
  ts_ssa <- Rssa::ssa(time_series, L = window_length)

  variance_explained <- ts_ssa$sigma^2/sum(ts_ssa$sigma^2)
  cumulative_variance_explained_total <- cumsum(variance_explained)
  cumulative_variance_explained_leave_off_one <- cumsum(variance_explained[2:length(variance_explained)])/(1-variance_explained[1])

  #a maximum index is determined using the proportion-of-variance-explained cutoff
  if(proportion_of_variance_type == "total"){
    max_eigenvector <- min(which(cumulative_variance_explained_total > max_proportion_of_variance))
  }
  if(proportion_of_variance_type == "leave_out_first"){
    max_eigenvector <- min(which(cumulative_variance_explained_leave_off_one > max_proportion_of_variance))
  }

  # Grouping ------------------------------------------------------
  # Assuming the first component is trend, calculate the period of
  # each pair of consecutive eigenvectors. Then, use abs. log ratio to compare to suspected periods.
  # Assign to period according to minimizing abs. log ratio.

  npairs <- max_eigenvector-2 #number of pairs to check. If < 1, then there are no pairs to check.
  any_pairs <- npairs >= 1
  #components are considered trend by default, and will later be assigned to seasonality if a period is identified
  default_grouping <- c(numeric(length(suspected_periods)),1)

  #grouping_matrix is used to classify components
  grouping_matrix <- rep(default_grouping,max_eigenvector)
  grouping_matrix <- matrix(grouping_matrix,nrow = max_eigenvector, byrow = TRUE)
  colnames(grouping_matrix) <- c(suspected_periods,"Trend")

  if(any_pairs){

    #period_info stores all the statistics for period similarities.
    #each row is for each distinct pair, starting with (2,3).
    #the first column is the first eigenvalue in the pair.
    #the second to the second last column store the statistics for period similarities.
    #the last column is whether or not that pair should be excluded from further consideration.


    period_info <- matrix(NA, nrow=npairs, ncol=(length(suspected_periods)+2))
    colnames(period_info) <- c("Period",suspected_periods,"Exclude")
    period_info[,1] <- 2:(max_eigenvector-1)
    period_info[,(length(suspected_periods)+2)] <- 0

    for(i in 2:(max_eigenvector-1)){
      #period_estimate is set to infinity when the estimate gives an error, warning, or
      #when the log-ratio statistic exceeds the pre-specified cut-off.
      period_estimate <- tryCatch(Rssa::parestimate(ts_ssa, groups = list(c(i,i+1)))$period[1],
                                  warning = function(w){period_estimate <- Inf}, error = function(e){period_estimate <- Inf})
      # period_estimate <- suppressWarnings(Rssa::parestimate(ts_ssa, groups = list(c(i,i+1)))$period[1])
      logratios <- abs(log(period_estimate/suspected_periods))
      logratios[which(logratios > log_ratio_cutoff)] <- Inf
      period_info[(i-1),2:(length(suspected_periods)+1)] <- logratios
    }

    #logratios_1 is a vector storing the first eigenvalues for the pairs to be examined.
    logratios_1 <- period_info[,1]

    #a for-loop to identify appropriate eigenvalue pairings.
    #the for-loop checks all the statistics stored in period_info columnwise.
    #it does the search using forward search. The search is terminated once the log-ratio statistic stops decreasing.
    #once a candidate pair is identified, that pair and the adjacent pairs are excluded from further considerations.
    for(j in 2:(length(suspected_periods)+1))
    {
      #logratios_j gives the log-ratio statistics for the j-th column (j-1 th suspected period).
      #logratios_exclude gives information about whether or not the pairs under consideration should be excluded.
      #regular_j is a vector indicating locations of the "regular" pairs.
      #These "regular" pairs are the ones whose log-ratio statistics are less than infinity and not excluded.
      logratios_j <- period_info[,j]
      logratios_exclude <- period_info[,"Exclude"]
      regular_j <- which((logratios_j < Inf)*(logratios_exclude==0)==1)

      #the (j-1)th suspected period (in the j-th column) is further examined if there is at least one "regular" pair.
      if(length(regular_j) > 0)
      {
        logratios_j_finite <- logratios_j[regular_j]
        logratios_1_j_finite <- logratios_1[regular_j]
        logratios_exclude_j_finite <- logratios_exclude[regular_j]

        #the case when there is only one "regular" pair.
        if(length(regular_j)==1)
        {
          #Exclude that pair.
          period_info[(logratios_1_j_finite-1),"Exclude"] <- 1
          #Set the eigenvalue entries to 1 for the correspondiing pairs and to 0 for the trend in grouping_matrix.
          grouping_matrix[c(logratios_1_j_finite, logratios_1_j_finite+1),(j-1)] <- 1
          grouping_matrix[c(logratios_1_j_finite, logratios_1_j_finite+1),"Trend"] <- 0

          #Exclude the adjacent pairs depending on where the "regular" pair is located.
          if(logratios_1_j_finite <= max(logratios_1)-1)
          {
            period_info[logratios_1_j_finite,"Exclude"] <- 1
          }
          if(logratios_1_j_finite >= min(logratios_1)+1)
          {
            period_info[(logratios_1_j_finite-2),"Exclude"] <- 1
          }
        }
        else{ #length(regular_j) > 1, meaning that when there is more than one "regular" case.
          #check_j becomes false when the log-ratio statistic stops decreasing or when it reaches the end.
          check_j <- TRUE
          #index_j keeps track of the location of the "regular" case.
          index_j <- 1
          while(check_j == TRUE)
          {
            #check to see if the log-ratio statistic decreases. If not, check_j becomes false.
            check_j <- (logratios_j_finite[index_j] > logratios_j_finite[(index_j+1)])
            if(check_j == TRUE)
            {
              index_j <- index_j + 1

              #when index_j reaches the end.
              if(index_j == length(regular_j))
              {
                check_j <- FALSE

                #Exclude that pair.
                period_info[logratios_1_j_finite[index_j]-1,"Exclude"] <- 1
                #Set the eigenvalue entries to 1 for the correspondiing pairs and to 0 for the trend in grouping_matrix.
                grouping_matrix[c(logratios_1_j_finite[index_j], logratios_1_j_finite[index_j]+1),(j-1)] <- 1
                grouping_matrix[c(logratios_1_j_finite[index_j], logratios_1_j_finite[index_j]+1),"Trend"] <- 0

                #Exclude the adjacent pairs depending on where the "regular" pair is located.
                if(logratios_1_j_finite[index_j] <= max(logratios_1)-1)
                {
                  period_info[logratios_1_j_finite[index_j],"Exclude"] <- 1
                }
                if(logratios_1_j_finite[index_j] >= min(logratios_1)+1)
                {
                  period_info[logratios_1_j_finite[index_j]-2,"Exclude"] <- 1
                }
              }
            }
            else #check_j == FALSE, i.e., when the forward search is terminated.
            {
              #Exclude that pair.
              period_info[logratios_1_j_finite[index_j]-1,"Exclude"] <- 1
              #Set the eigenvalue entries to 1 for the correspondiing pairs and to 0 for the trend in grouping_matrix.
              grouping_matrix[c(logratios_1_j_finite[index_j], logratios_1_j_finite[index_j]+1),(j-1)] <- 1
              grouping_matrix[c(logratios_1_j_finite[index_j], logratios_1_j_finite[index_j]+1),"Trend"] <- 0

              #Exclude the adjacent pairs depending on where the "regular" pair is located.
              if(logratios_1_j_finite[index_j] <= max(logratios_1)-1)
              {
                period_info[logratios_1_j_finite[index_j],"Exclude"] <- 1
              }
              if(logratios_1_j_finite[index_j] >= min(logratios_1)+1)
              {
                period_info[logratios_1_j_finite[index_j]-2,"Exclude"] <- 1
              }
            }

          }

        }

      }
    }

  }
  #trend_indices provide locations of the eigenvalues corresponding to trend component(s).
  trend_indices <- which(grouping_matrix[,"Trend"] == 1)

  #when the number of identified trend components is less than num_trend_components,
  #set num_trend_components equal to the number of identified trend components.
  if(length(trend_indices) < num_trend_components){
    num_trend_components <- length(trend_indices)
  }

  #pick only the first non-periodic num_trend_components eigenvalues as trend components.
  trend_indices <- trend_indices[1:num_trend_components]
  trend_column <- rep(0,dim(grouping_matrix)[1])
  trend_column[trend_indices] <- 1
  grouping_matrix[,"Trend"] <- trend_column

  #print(grouping_matrix)

  trend_component <- Rssa::reconstruct(ts_ssa, groups = list(which(grouping_matrix[,"Trend"] == 1)))$F1

  reconstruction_matrix <- matrix(nrow = n, ncol = length(suspected_periods)+2) #one for each period, trend, and total seasonality

  reconstruction_list <- list()

  reconstruction_list[["Main_Trend"]] <- Rssa::reconstruct(ts_ssa, groups = list(c(1)))$F1

  reconstruction_list[["Trend"]] <- Rssa::reconstruct(ts_ssa, groups = list(which(grouping_matrix[,"Trend"] == 1)))$F1

  total_seasonality <- rep(0,n)
  for(i in 1:length(suspected_periods)){
    period_seasonality <-  Rssa::reconstruct(ts_ssa,
                                             groups = list(which(grouping_matrix[,i]==1)))$F1
    reconstruction_list[[paste("Period.",suspected_periods[i])]] <- period_seasonality
    total_seasonality <- total_seasonality+period_seasonality
  }

  reconstruction_list[["Seasonality"]] <- total_seasonality

  decomp <- new_decomposition(reconstruction_list = reconstruction_list,
                              grouping_matrix = grouping_matrix,
                              window_length = window_length,
                              ts_ssa = ts_ssa)


  return(decomp)

}

