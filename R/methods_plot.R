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
                  lwd = 4.5, type ="l")

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
      lines(x$reconstruction[[1]],col = "red", lwd = 5)
      if(legend){
      legend("topright", c("Trend","Trend + Seasonality"),col = c("red","black"),lwd = c(2,1.5))
      }
    }
}

#' @title visitation_forecast Plot Methods
#' @description Methods for plotting objects of the class "visitation_forecast".
#' @import graphics
#' @importFrom methods hasArg
#' @import ggplot2
#' @export
#' @param x An object of class "visitation_forecast".
#' @param difference A Boolean specifying whether to plot the original fit or differenced series. The default option is FALSE, in which case, the series is not differenced.
#' @param log_outputs A Boolean specifying whether to plot the outputs in standard scale or the logged outputs
#' @param actual_visitation A time series object representing the actual visitation in the standard scale
#' @param plot_points A Boolean specifying whether to plot the individual points of visitation
#' @param xlab A String to overwrite the x label of the graph
#' @param ylab A String to overwrite the y label of the graph
#' @param main A String to overwrite the main label of the graph
#' @param pred_color A String to specify the color of the line for predicted visitation
#' @param actual_color A String to specify the color of the line for actual visitation
#' @param size A number representing the size of the line to plot. 



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

#
plot.visitation_forecast <- function(x, difference = FALSE, log_outputs = FALSE, actual_visitation = NULL, xlab = "Time", ylab = "Fitted Value", pred_color = "#228B22", actual_color = "#FF0000", size = 1.5, main = "Forecasts for Visitation Model", plot_points = FALSE, ...){

  series_to_plot <- x$forecasts
  y<-NULL
  group <-NULL
  if(log_outputs)
  { 
    if(difference)
    {
      ylab <- paste(ylab,"(differenced)")
      main <- paste(main, "(differenced)")
      series_to_plot <- x$differenced_logged_forecasts
    }
    else
    {
      series_to_plot <- x$logged_forecasts
    }
  }
  else{
    if(difference)
    {
      ylab <- paste(ylab,"(differenced)")
      main <- paste(main, "(differenced)")
      series_to_plot <- x$differenced_standard_forecasts
    }
  }

 actual_visitation_data_frame <-NULL
 plot_actual_visitation <- FALSE
 data_frame_to_plot <-fortify(series_to_plot, is.date = TRUE) #need to convert ts objec to a dataframe
 data_frame_to_plot$group <- rep("forecast", length(data_frame_to_plot$x))

if(!is.null(actual_visitation))
{
  if(is.ts(actual_visitation))
  {
    plot_actual_visitation <-TRUE

    if(log_outputs || difference)
    {
      actual_visitation <-log(actual_visitation)
    }

    if(difference)
    {
      actual_visitation <-diff(actual_visitation)

      if(!log_outputs)
      {
        actual_visitation <-exp(actual_visitation)
      }
    }

    actual_visitation_data_frame  <-fortify(actual_visitation, is.date = TRUE)
    actual_visitation_data_frame$group <- rep("actual", length(actual_visitation_data_frame$x))
    data_frame_to_plot<-rbind(data_frame_to_plot,actual_visitation_data_frame)
  }
  else
  {
    stop("ERROR while plotting visitation predictions, the provided actual_visitation object needs to be of type time series")
  }
}

 ggplot(data=data_frame_to_plot, aes( x = x, y = y,color = group)) + geom_line(size = size) + theme_bw() +labs(y= ylab, x = xlab, title = main)+theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text( size=13), axis.text.y = element_text(size=13)) + {if(plot_points)geom_point(size = 3)} +
 scale_x_continuous(breaks = round(seq(min(data_frame_to_plot$x), max(data_frame_to_plot$x), by = ((max(data_frame_to_plot$x) - min(data_frame_to_plot$x))/10)),1))+ {scale_color_manual(label = "", values = c("forecast" = pred_color, "actual" = actual_color))} 

}

construct_visitation_forecast_cumsum_df <- function(labeled_visitation_forecast)
{
  visitation_label <-labeled_visitation_forecast$label
  visitation_forecast <- labeled_visitation_forecast$visitation_forecast

  forecast <- 100*(visitation_forecast$differenced_logged_forecasts)
  forecast_df = fortify(forecast)
  forecast_df$y <- cumsum(forecast_df$y)
  forecast_df$group <-rep(visitation_label, length(forecast_df$x))

  return (forecast_df)

}

construct_visitation_forecast_percent_change_df <-function(labeled_visitation_forecast)
{
  visitation_label <-labeled_visitation_forecast$label
  visitation_forecast <- labeled_visitation_forecast$visitation_forecast

  forecast <- 100*(visitation_forecast$differenced_logged_forecasts)
  forecast_df = fortify(forecast)
  forecast_df$group <-rep(visitation_label, length(forecast_df$x))

  return (forecast_df)
}

construct_visitation_forecast_df <- function(labeled_visitation_forecast, difference, log_outputs)
{

  label <- labeled_visitation_forecast$label
  visitation_forecast <-labeled_visitation_forecast$visitation_forecast
 
  forecast <- visitation_forecast$forecasts

  if(log_outputs)
  { 
    if(difference)
    {
      forecast <- visitation_forecast$differenced_logged_forecasts
    }
    else
    {
      forecast <- visitation_forecast$logged_forecasts
    }
  }
  else if(difference){
      forecast <- visitation_forecast$differenced_standard_forecasts
  }

 forecast_df = fortify(forecast)
 forecast_df$group <- rep(label, length(forecast_df$x))

  return (forecast_df)
}


construct_actual_df <-function(actual_visitation_ts, difference, log_outputs, label, plot_cumsum = FALSE, plot_percent_change = FALSE )
{
 
  if(!is.null(actual_visitation_ts))
  {
    if(is.ts(actual_visitation_ts))
    {

      if(plot_cumsum || plot_percent_change)
      {
       log_outputs <- TRUE
       difference <- TRUE
       plot_percent_change <- TRUE
      }

      if(log_outputs || difference)
      {
        actual_visitation_ts <- log(actual_visitation_ts)
      }

      if(difference)
      {
        actual_visitation_ts <- diff(actual_visitation_ts)

        if(!log_outputs)
        {
          actual_visitation_ts <- exp(actual_visitation_ts)
        }
      }

      if(plot_percent_change)
      {
        actual_visitation_ts <-100*(actual_visitation_ts)
      }

      actual_visitation_df  <-fortify(actual_visitation_ts)

      if(plot_cumsum)
      {
        actual_visitation_df$y <- cumsum(actual_visitation_df$y)
      }

      actual_visitation_df$group <- rep(label, length(actual_visitation_df$x))

      return (actual_visitation_df)
    }
    else
    {
      stop("ERROR while plotting visitation predictions, the provided actual_visitation object needs to be of type time series")
    }
  }

  return (list(x =c(), y = c(), group = c()))
}


#' @title visitation_forecast_ensemble Plot Methods
#' @description Methods for plotting objects of the class "visitation_forecast_ensemble".
#' @import graphics
#' @importFrom methods hasArg
#' @import ggplot2
#' @export
#' @param x An object of class "visitation_forecast_ensemble".
#' @param difference A Boolean specifying whether to plot the original fit or differenced series. The default option is FALSE, in which case, the series is not differenced.
#' @param log_outputs A Boolean specifying whether to plot the outputs in standard scale or the logged outputs
#' @param plot_cumsum A Boolean specifying whether to plot the cumsum of the outputs
#' @param plot_percent_change A Boolean specifying whether to plot the percent change of the outputs
#' @param actual_visitation A time series object representing the actual visitation in the standard scale
#' @param actual_visitation_label A Label for the actual visitation in the legend
#' @param plot_points A Boolean specifying whether to plot the individual points of visitation
#' @param xlab A String to overwrite the x label of the graph
#' @param ylab A String to overwrite the y label of the graph
#' @param main A String to overwrite the main label of the graph
#' @param pred_colors An array of strings specifying the colors for the predicted outputs.
#' @param size A number representing the size of the line to plot. 
#' @param actual_color A String to specify the color of the line for actual visitation
#' @param ... Additional arguments.
#' @examples
#' data("park_visitation")
#' data("flickr_userdays")
#' YELL_data <- park_visitation[park_visitation$park == "YELL",]
#' YELL_data
#'
#' YELL_PUD <- YELL_data$pud #PUD Data
#' YELL_NPS <- YELL_data$nps #NPS Data
#'
#' #The YELL data from 2005 through 2016 are used as the training data.
#' YELL_data[1:144,]$date
#' YELL_PUD.train <- ts(YELL_PUD[1:144], start = c(2005,1), end = c(2016,12), freq = 12)
#' YELL_NPS.train <- ts(YELL_NPS[1:144], start = c(2005,1), end = c(2016,12), freq = 12)
#'
#'
#' YELL_NPS.test <- ts(YELL_NPS[144:length(YELL_NPS)],
#'                    start = c(2016,12), end = c(2017,12), freq = 12)
#'
#' #Construct models without linear trend (with or without OSC).
#' YELL_model.without_trend <- visitation_model(onsite_usage = YELL_PUD.train,
#'                                              ref_series = YELL_NPS.train,
#'                                              parameter_estimates = "joint", trend = "none")
#'
#' YELL_model.without_trend_and_NPS <- visitation_model(onsite_usage = YELL_PUD.train,
#'                                                      ref_series = NULL)
#'
#' YELL_pred.without_trend <- predict(YELL_model.without_trend,n_ahead = 12)
#' YELL_pred.without_trend_and_NPS <- predict(YELL_model.without_trend_and_NPS, n_ahead = 12)
#' YELL_NPS.test
#'
#'
#'
#' forecast_ensemble <- new_visitation_forecast_ensemble(list(YELL_pred.without_trend ,
#' YELL_pred.without_trend_and_NPS), list("Without Trend", "Without Trend and NPS"))
#'
#' #Plot the forecasts and actual percent changes for 2017.
#' plot(forecast_ensemble, actual_visitation = YELL_NPS.test,
#'      ylab = "Percent Change in Monthly Visitation", main = "Forecast vs. 
#' Actual Monthly Percent Change in Visitation in 2017", plot_percent_change = TRUE)
#'
#'
#' #Plot the cumulative forecasts and actual cumulative percent changes for 2017.
#' plot(forecast_ensemble, actual_visitation = YELL_NPS.test,
#'      ylab = "Cumulative Percent Change in Monthly Visitation", 
#' main = "Forecast vs. Actual Monthly Percent Change in Visitation in 2017", plot_cumsum = TRUE)





plot.visitation_forecast_ensemble<- function(x, difference = FALSE, log_outputs = FALSE, plot_cumsum = FALSE, plot_percent_change = FALSE, actual_visitation = NULL, actual_visitation_label = "Actual", xlab = "Time", ylab = "Fitted Value", pred_colors = c("#ff6361", "#58508d","#bc5090", "#003f5c" ), actual_color = "#ffa600", size = 1.5, main = "Forecasts for Visitation Model", plot_points = FALSE, ...)
{
  forecasts <- x$forecast_ensemble
  y <- NULL
  group <- NULL #adding these placeholdesr because CRAN is dumb and thinks that aes() is using globals, but if you change the syntax to not get mad then ggplot starts throwing warnings I can't supress. So this will just have to do. 


  if(length(forecasts) > length(pred_colors))
  {
    stop("ERROR : provided number of forecasts is larger than the default number of colors, please provide a pred_color list")
  }

  if(difference)
  {
     ylab <- paste(ylab,"(differenced)")
     main <- paste(main, "(differenced)")
  }

  data_frame_to_plot <- data.frame(x = c(), y=c(), group=c())

  all_labels <-c()
  df_list = list()

  for(i in seq_along(forecasts))
  {

    if(plot_percent_change)
    {
      df_list[[i]] = construct_visitation_forecast_percent_change_df(forecasts[[i]])
    }
    else if(plot_cumsum)
    {
      df_list[[i]] = construct_visitation_forecast_cumsum_df(forecasts[[i]])
    }
    else 
    {
       df_list[[i]] = construct_visitation_forecast_df(forecasts[[i]], difference, log_outputs)
    }
    all_labels =c(forecasts[[i]]$label, all_labels)
  }

  all_labels = c(all_labels, actual_visitation_label)
  pred_colors[[length(all_labels)]] = actual_color


  data_frame_to_plot<-do.call(rbind, df_list)

  data_frame_to_plot<-rbind(data_frame_to_plot, construct_actual_df(actual_visitation, difference, log_outputs, actual_visitation_label, plot_cumsum=plot_cumsum, plot_percent_change = plot_percent_change))

  theme =  theme_gray()+ theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom",
            legend.direction = "horizontal", axis.text.x = element_text( size=13), axis.text.y = element_text(size=13))

  scale = scale_x_continuous(breaks = round(seq(min(data_frame_to_plot$x), max(data_frame_to_plot$x), by = ((max(data_frame_to_plot$x) - min(data_frame_to_plot$x))/10)),1))



  line = geom_line(size = 1.5)

  labels = labs(y= ylab, x = xlab,
      title = main)

  legend = {scale_color_manual(name = "",
          values = setNames(pred_colors,all_labels))}

  # plot the the frame

  ggplot(data=data_frame_to_plot, aes(x = x, y = y, color = group))+ theme + scale + line + labels + legend

}




#' @title visitation_model Plot Methods
#' @description Methods for plotting objects of the class "decomposition".
#' @import graphics
#' @importFrom methods hasArg
#' @import ggplot2
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

