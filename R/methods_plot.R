#' @title Decomposition Plot Methods
#' @description Methods for plotting objects of the class "decomposition".
#' @import graphics
#' @importFrom methods hasArg
#' @export
#' @param x An object of class "decomposition".
#' @param type A character string. One of "full","period", or "classical". If "full", the full reconstruction is plotted. If "period", the reconstruction of each period is plotted individually. If "classical", the trend and seasonality are plotted.
#' @param legend A Boolean specifying whether a legend should be added when type is "full". The default option is TRUE.
#' @param ... Additional arguments.
#'
#' @return A plot of the reconstruction in the "decomposition" class object.
#' @examples
#' data("park_visitation")
#'
#' park <- "YELL"
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, frequency = 12)
#' nps_ts <- log(nps_ts)
#'
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, frequency = 12)
#' pud_ts <- log(pud_ts)
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, frequency = 12)
#' nps_ts <- log(nps_ts)
#'
#'
#'
#' decomposition_pud <- auto_decompose(pud_ts)
#' decomposition_nps <- auto_decompose(nps_ts)
#'
#' plot(decomposition_pud,lwd = 2)
#' plot(decomposition_pud,type = "period")
#' plot(decomposition_pud,type = "classical")
#'
#'
#' plot(decomposition_nps,legend = TRUE)
#'
#'
#' plot(decomposition_nps,type = "period")
#' plot(decomposition_nps,type = "classical")
#'

#####################################################################

plot.decomposition <- function(x, type = c("full","period","classical"), legend = TRUE,...){

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  main <- ylim <- NULL #global variables; possible user inputs

  type <- match.arg(type)
  l <- length(x$reconstruction)

  presets <- list(ylab = "Reconstruction",
                  lwd = 1.5)

  z <- list(...)
  new_args <- names(z)

  presets[new_args] <- z


    if(identical(type,"period")){

      ncomponents <- length(x$reconstruction)-2
      components <- 2:(ncomponents+1)

      nrow <- ceiling(sqrt(ncomponents))
      ncol <- floor(sqrt(ncomponents))
      par(mfrow = c(nrow,ncol))


        for(i in components){
          presets[c("main","x")] <- list(names(x$reconstruction)[i],x$reconstruction[[i]])
          do.call("plot",presets)
        }


      }

    if(identical(type,"classical")){

      Trend <- x$reconstruction$Trend
      Seasonality <- x$reconstruction$Seasonality
      Reconstruction <- Trend+Seasonality

      # ylimits should default to be the same size but centered on each component for fair comparisons
      differences <- c(max(Trend)-min(Trend),
                       max(Seasonality)-min(Seasonality),
                       max(Reconstruction)-min(Reconstruction))

      half_max_difference <- max(differences)/2


      components <- list(Trend = Trend,
                         Seasonality = Seasonality,
                         Reconstruction = Reconstruction)

      par(mfrow = c(3,1))

      for(i in 1:3){
        comp_val <- components[[i]]
        comp_mid <- (max(comp_val)+min(comp_val))/2

        presets[c("main","x","ylim")] <- list(names(components)[[i]],comp_val,c(comp_mid-half_max_difference,comp_mid+half_max_difference))
        presets[new_args] <- z
        do.call("plot",presets)
      }

    }

    if(type == "full"){

      reconstruction <- x$reconstruction[[1]]+x$reconstruction[[l]]
      y_lb <- min(reconstruction)
      y_ub <- max(reconstruction)
      y_margin <- (y_ub-y_lb)/4 #add margin to make room for legend

      presets["x"] <- list(reconstruction)
      if(!hasArg(main)){presets["main"] = "Trend and Seasonality"}else{presets["main"] = z$main}
      if(!hasArg(ylim)){presets["ylim"] = list(c(y_lb,y_ub+y_margin))}else{presets["ylim"] = list(z$ylim)}

      par(mfrow = c(1,1))
      do.call("plot",presets)
      lines(x$reconstruction[[1]],col = "red", lwd = 2)
      if(legend){
      legend("topright", c("Trend","Trend + Seasonality"),col = c("red","black"),lwd = c(2,1.5))
      }
    }
}

#' @title visitation_forecast Plot Methods
#' @description Methods for plotting objects of the class "visitation_forecast".
#' @import graphics
#' @importFrom methods hasArg
#' @export
#' @param x An object of class "decomposition".
#' @param type A character string. One of "full","period", or "classical". If "full", the full reconstruction is plotted. If "period", the reconstruction of each period is plotted individually. If "classical", the trend and seasonality are plotted.
#' @param difference A Boolean specifying whether to plot the original fit or differenced series. The default option is FALSE, in which case, the series is not differenced.
#' @param ... Additional arguments.
#'
#' @return No return value, called for plotting objects of the class "visitation_forecast".
#'
#' @examples
#' #' #Example:
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
#' vf <- predict(mf,12, only_new = TRUE)
#' plot(vf)

plot.visitation_forecast <- function(x, type = c("fitted"), difference = FALSE,...){

  type = match.arg(type)

  series_to_plot <- c()
  if(identical(type,"fitted")){
    series_to_plot <- x$forecasts
    ylab <- "Fitted Value"
    main <- "Forecasts for visitation model"
  }
  if(difference){
    series_to_plot <- diff(series_to_plot)
    ylab <- paste(ylab,"(differenced)")
    main <- paste(main, "(differenced)")
  }

  presets <- list(x = series_to_plot,
                  ylab = ylab,
                  main = main,
                  lwd = 1)
  z <- list(...)
  new_args <- names(z)

  presets[new_args] <- z

  do.call(plot,presets)

}

#' @title visitation_model Plot Methods
#' @description Methods for plotting objects of the class "decomposition".
#' @import graphics
#' @importFrom methods hasArg
#' @export
#' @param x An object of class "decomposition".
#' @param type A character string. One of "full","period", or "classical". If "full", the full reconstruction is plotted. If "period", the reconstruction of each period is plotted individually. If "classical", the trend and seasonality are plotted.
#' @param difference A Boolean specifying whether to plot the original fit or differenced series. The default option is FALSE, in which case, the series is not differenced.
#' @param ... Additional arguments.
#'
#' @return No return value, called for plotting objects of the class "decomposition".
#'
#' @examples
#'
#'
#'data("park_visitation")
#'data("flickr_userdays")
#'
#' park <- "YELL"
#' pud_ts <- ts(park_visitation[park_visitation$park == park,]$pud, start = 2005, freq = 12)
#' pud_ts <- log(pud_ts)
#'
#' nps_ts <- ts(park_visitation[park_visitation$park == park,]$nps, start = 2005, freq = 12)
#' nps_ts <- log(nps_ts)
#'
#' nps_decomp <- auto_decompose(nps_ts)
#'
#' trend_proxy <- log(flickr_userdays)
#'
#' vm <- visitation_model(pud_ts,trend_proxy,ref_series = nps_ts)
#' plot(vm)

plot.visitation_model <- function(x, type = c("fitted"), difference = FALSE,...){

  type = match.arg(type)

  series_to_plot <- c()
  if(identical(type,"fitted")){
    series_to_plot <- x$visitation_fit
    ylab <- "Fitted Value"
    main <- "Fitted values for visitation model"
  }
  if(difference){
    series_to_plot <- diff(series_to_plot)
    ylab <- paste(ylab," (differenced)")
  }

  presets <- list(x = series_to_plot,
                  ylab = ylab,
                  main = main,
                  lwd = 1)
  z <- list(...)
  new_args <- names(z)

  presets[new_args] <- z

  do.call(plot,presets)

}


