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

#' @title Decomposition ggplot Method
#' @description Methods for plotting objects of the class "decomposition".
#' @import ggplot2 cowplot
#' @importFrom methods hasArg
#' @importFrom ggplot2 ggplot
#' @export
#' @param data An object of class "decomposition".
#' @param mapping Default list of aesthetic mappings to use for plot. If not specified, must be supplied in each layer added to the plot.
#' @param type A character string. One of "full","period", or "classical". If "full", the full reconstruction is plotted. If "period", the reconstruction of each period is plotted individually. If "classical", the trend and seasonality are plotted.
#' @param size A number that represents the thickness of the lines being plotted
#' @param plot_points a boolean to specify if the plot should be points or continous line.
#' @param date_breaks A string to represent the distance between dates that the x-axis should be in. ex "1 month", "1 year"
#' @param date_labels A string to represent the format of the x-axis time labels.
#' @param ... Additional arguments.
#' @return A plot of the reconstruction in the "decomposition" class object.

  ggplot.decomposition <- function(data, mapping = aes(), type = c("full", "period", "classical"), size = 1.5, date_breaks = "2 year", date_labels = "%y", plot_points = FALSE, ...) {
    type <- match.arg(type)
    reconstruction <- data$reconstruction
    theme =  ggplot2::theme_gray()+ theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom",
                                          legend.direction = "horizontal", axis.text.x = element_text( size=15), axis.text.y = element_text(size=17))
    if(type == "full")
    {
      plot_full_decomposition(reconstruction$Main_Trend, reconstruction$Seasonality, size, date_breaks, date_labels, plot_points,theme)
    }
    else if (type == "classical")
    {
      plot_classical_decomposition(reconstruction$Trend, reconstruction$Seasonality, size, date_breaks, date_labels, plot_points, theme)
    }
    else if (type == "period")
    {
      plot_period_decomposition(reconstruction, size, date_breaks, date_labels, plot_points, theme)
    }
  }

  plot_period_decomposition <- function(reconstruction, size, date_breaks, date_labels, plot_points, theme)
  {
    ncomponents <- length(reconstruction)-2
    components <- 2:(ncomponents+1)

    nrow <- ceiling(sqrt(ncomponents))
    ncol <- floor(sqrt(ncomponents))
    plot_list <- list()

    ylab <- "reconstruction"
    xlab <- "Time"

    color = "black"
    plot_to_do <- NULL
    for(i in components){
      ts_to_plot <- reconstruction[[i]]
      main <- names(reconstruction)[i]
      df_to_plot <- convert_ts_forecast_to_df(ts_to_plot)

      scale = scale_x_continuous(breaks = round(seq(min(df_to_plot$x),
                                                    max(df_to_plot$x), by = 0.1),1))
      line = geom_line(size = 1.5)
      labels = labs(y= ylab, x = xlab,
                          title = main)
      new_plot <- ggplot(data=df_to_plot, aes_string(x ='x', y = 'y'))+ theme + scale + line + labels +scale_x_date(date_breaks = date_breaks,date_labels = date_labels)
      plot_list[[length(plot_list)+1]] = new_plot
    }

    plot_grid(plotlist = plot_list, nrow = nrow, ncol = ncol)
  }


  plot_classical_decomposition <- function(trend, seasonality, size, date_breaks, date_labels, plot_points, theme)
  {
    main_trend <- "Trend"
    main_seasonality <- "Seasonality"
    main_reconstruction <- "Reconstruction"
    ylab <- "reconstruction"
    xlab <- "Time"
    color = "black"
    trend_df <- convert_ts_forecast_to_df(trend)
    seasonality_df <-convert_ts_forecast_to_df(seasonality)
    reconstruction_df <-convert_ts_forecast_to_df(trend+seasonality)

    scale = scale_x_continuous(breaks = round(seq(min(trend_df$x,seasonality_df$x, reconstruction_df$x),
                                                  max(trend_df$x,seasonality_df$x, reconstruction_df$x), by = 0.1),1))
    line = geom_line(size = 1.5)
    labels_trend = labs(y= ylab, x = xlab,
                  title = main_trend)

    trend_plot <- ggplot(data=trend_df, aes_string(x ='x', y = 'y'))+ theme + scale + line + labels_trend +scale_x_date(date_breaks = date_breaks,date_labels = date_labels)


    labels_seasonality = labs(y= ylab, x = xlab,
                        title = main_seasonality)

    seasonality_plot <- ggplot(data=seasonality_df, aes_string(x ='x', y = 'y'))+ theme + scale + line + labels_seasonality +scale_x_date(date_breaks = date_breaks,date_labels = date_labels)

    labels_reconstruction = labs(y= ylab, x = xlab,
                        title = main_reconstruction)
    reconstruction_plot <- ggplot(data=reconstruction_df, aes_string(x ='x', y = 'y'))+ theme + scale + line + labels_reconstruction +scale_x_date(date_breaks = date_breaks,date_labels = date_labels)

    plot_grid(
      trend_plot, seasonality_plot, reconstruction_plot, ncol = 1
    )
}

plot_full_decomposition <- function(trend, seasonality, size, date_breaks, date_labels, plot_points, theme)
{
    main <- "Trend and Seasonality"
    ylab <- "reconstruction"
    xlab <- "Time"

    trend_color = "red"
    trend_seasonality_color = "black"

    trend_label <- "Trend"
    trend_seasonality_label <- "Trend + Seasonality"

    trend_df <- convert_ts_forecast_to_df(trend)
    trend_df$group <- rep(trend_label, length(trend_df$x))

    trend_seasonality_df <-convert_ts_forecast_to_df(trend+seasonality)
    trend_seasonality_df$group <- rep(trend_seasonality_label, length(trend_seasonality_df$x))

    all_labels <- c(trend_label, trend_seasonality_label)
    data_frame_to_plot<-rbind(trend_df,trend_seasonality_df )
    scale = scale_x_continuous(breaks = round(seq(min(data_frame_to_plot$x),
                                                  max(data_frame_to_plot$x), by = 0.1),1))
    line = geom_line(size = 1.5)
    labels = labs(y= ylab, x = xlab,
                  title = main)
    legend = {scale_color_manual(name = "",
                                 values = setNames(c(trend_color, trend_seasonality_color), all_labels))}
    ggplot(data=data_frame_to_plot, aes_string(x ='x', y = 'y',color = 'group'))+ theme + scale + line + labels + legend +scale_x_date(date_breaks = date_breaks,date_labels = date_labels)
}

#' @title visitation_forecast Plot Method
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

#' @title visitation_forecast ggPlot Method
#' @description Methods for plotting objects of the class "visitation_forecast".
#' @import ggplot2
#' @importFrom ggplot2 ggplot
#' @export
#' @param data An object of the "visitation_forecast" class.
#' @param difference A boolean to plot the differenced series.
#' @param mapping Default list of aesthetic mappings to use for plot. If not specified, must be supplied in each layer added to the plot.
#' @param log_outputs A boolean to plot the logged outputs of the forecast.
#' @param actual_visitation A timeseries object representing the actual visitation that will be plotted along site the visitation_forecast object.
#' @param xlab A string that will be used for the xlabel of the plot.
#' @param ylab A string that will be used for the ylabel of the plot.
#' @param pred_color a String that will be used for the predicted series color of the plot.
#' @param actual_color a String that will be used for the actual series color of the plot.
#' @param size A number that represents the thickness of the lines being plotted.
#' @param main A string that will be used for the title of the plot.
#' @param plot_points a boolean to specify if the plot should be points or continous line.
#' @param date_breaks A string to represent the distance between dates that the x-axis should be in. ex "1 month", "1 year".
#' @param date_labels A string to represent the format of the x-axis time labels. ex
#' @param ... extra arguments to pass in
#' @return No return value, called for plotting objects of the class "visitation_forecast".
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

ggplot.visitation_forecast <- function(data, mapping = aes(), difference = FALSE, log_outputs = FALSE, actual_visitation = NULL, xlab = "Time", ylab = "Fitted Value", pred_color = "#228B22", actual_color = "#FF0000", size = 1.5, main = "Forecasts for Visitation Model", plot_points = FALSE, date_breaks ="1 month", date_labels = "%y %b", ...){
  series_to_plot <- data$forecasts

  if(log_outputs)
  {
    if(difference)
    {
      ylab <- paste(ylab,"(differenced)")
      main <- paste(main, "(differenced)")
      series_to_plot <- data$differenced_logged_forecasts
    }
    else
    {
      series_to_plot <- data$logged_forecasts
    }
  }
  else{
    if(difference)
    {
      ylab <- paste(ylab,"(differenced)")
      main <- paste(main, "(differenced)")
      series_to_plot <- data$differenced_standard_forecasts
    }
  }

 actual_visitation_data_frame <-NULL
 plot_actual_visitation <- FALSE
 data_frame_to_plot <-convert_ts_forecast_to_df(series_to_plot) #need to convert ts objec to a dataframe
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

    actual_visitation_data_frame  <-convert_ts_forecast_to_df(actual_visitation)
    actual_visitation_data_frame$group <- rep("actual", length(actual_visitation_data_frame$x))
    data_frame_to_plot<-rbind(data_frame_to_plot,actual_visitation_data_frame)
  }
  else
  {
    stop("ERROR while plotting visitation predictions, the provided actual_visitation object needs to be of type time series")
  }
}

 ggplot(data=data_frame_to_plot, aes_string(x ='x', y = 'y',color = 'group')) + geom_line(size = size) + theme_bw() +labs(y= ylab, x = xlab, title = main)+theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text( size=9), axis.text.y = element_text(size=13)) + {if(plot_points)geom_point(size = 3)} +
 scale_x_continuous(breaks = round(seq(min(data_frame_to_plot$x), max(data_frame_to_plot$x), by = 0.1),1))+ {scale_color_manual(label = "", values = c("forecast" = pred_color, "actual" = actual_color))} + scale_x_date(date_breaks = date_breaks,date_labels = date_labels)

}

#' @title convert_ts_forecast_to_df
#' @description method for converting a timerseries to a dataframe so that it can be plotted with ggplot2 and keep a Date x-axis.
#' @import zoo
#' @export
#' @param forecast timeseries object to convert
convert_ts_forecast_to_df <- function(forecast)
{

  return (data.frame(x=as.Date(as.yearmon(time(forecast)), frac=1), y = as.vector(forecast)))
}


construct_visitation_forecast_cumsum_df <- function(labeled_visitation_forecast)
{
  visitation_label <-labeled_visitation_forecast$label
  visitation_forecast <- labeled_visitation_forecast$visitation_forecast

  forecast <- 100*(visitation_forecast$differenced_logged_forecasts)
  forecast_df = convert_ts_forecast_to_df(forecast)

  forecast_df$y <- cumsum(forecast_df$y)
  forecast_df$group <-rep(visitation_label, length(forecast_df$x))
  return (forecast_df)
}

construct_visitation_forecast_percent_change_df <-function(labeled_visitation_forecast)
{
  visitation_label <-labeled_visitation_forecast$label
  visitation_forecast <- labeled_visitation_forecast$visitation_forecast

  forecast <- 100*(visitation_forecast$differenced_logged_forecasts)
  forecast_df = convert_ts_forecast_to_df(forecast)
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

 forecast_df = convert_ts_forecast_to_df(forecast)
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

      actual_visitation_df  <-convert_ts_forecast_to_df(actual_visitation_ts)

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

#' @title visitation_model visitation_forecast_ensemble ggplot Methods
#' @description Method for plotting forecast ensemble with ggplot.
#' @export
#' @import ggplot2
#' @importFrom ggplot2 ggplot
#' @param data An object of class visitation_forecast_ensemble.
#' @param mapping Default list of aesthetic mappings to use for plot. If not specified, must be supplied in each layer added to the plot.
#' @param difference A Boolean specifying whether to plot the original fit or differenced series. The default option is FALSE, in which case, the series is not differenced.
#' @param log_outputs whether to log the outputted forecasts or not
#' @param plot_cumsum whether to plot the cumulative sum or not
#' @param plot_percent_change whether to plot the percent change or not
#' @param actual_visitation A timeseries object representing the actual visitation that will be plotted along site the visitation_forecast object
#' @param xlab A string that will be used for the xlabel of the plot
#' @param ylab A string that will be used for the ylabel of the plot
#' @param pred_colors an array of Strings that will be used for the predicted series colors of the plot
#' @param actual_color a String that will be used for the actual series color of the plot,
#' @param actual_visitation_label a string that will be used for the label of the actual visitation.
#' @param size A number that represents the thickness of the lines being plotted
#' @param main A string that will be used for the title of the plot
#' @param plot_points a boolean to specify if the plot should be points or continous line.
#' @param date_breaks A string to represent the distance between dates that the x-axis should be in. ex "1 month", "1 year"
#' @param date_labels A string to represent the format of the x-axis time labels.
#' @param ... extra arguments to pass in
#' @return No return value, called for plotting objects of the class "visitation_forecast".
#'
ggplot.visitation_forecast_ensemble<- function(data, mapping = aes(), difference = FALSE, log_outputs = FALSE, plot_cumsum = FALSE, plot_percent_change = FALSE, actual_visitation = NULL, actual_visitation_label = "Actual", xlab = "Time", ylab = "Fitted Value", pred_colors = c("#ff6361", "#58508d","#bc5090", "#003f5c" ), actual_color = "#ffa600", size = 1.5, main = "Forecasts for Visitation Model", plot_points = FALSE, date_breaks ="1 month", date_labels = "%y %b", ...)
{
  forecasts <- data$forecast_ensemble

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
  theme =  ggplot2::theme_gray()+ theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom",
            legend.direction = "horizontal", axis.text.x = element_text( size=9), axis.text.y = element_text(size=13))
  scale = scale_x_continuous(breaks = round(seq(min(data_frame_to_plot$x),
                                      max(data_frame_to_plot$x), by = 0.1),1))
  line = geom_line(size = 1.5)
  labels = labs(y= ylab, x = xlab,
      title = main)
  legend = {scale_color_manual(name = "",
          values = setNames(pred_colors,all_labels))}
  # plot the the frame
  ggplot(data=data_frame_to_plot, aes_string(x ='x', y = 'y',color = 'group'))+ theme + scale + line + labels + legend +scale_x_date(date_breaks = date_breaks,date_labels = date_labels)
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

#' @title visitation_model ggplot2 method
#' @description Methods for plotting objects of the class "decomposition" with ggplot2.
#' @export
#' @importFrom ggplot2 ggplot
#' @param data An object of class "decomposition".
#' @param mapping Default list of aesthetic mappings to use for plot. If not specified, must be supplied in each layer added to the plot.
#' @param difference A Boolean specifying whether to plot the original fit or differenced series. The default option is TRUE, in which case, the series is differenced.
#' @param actual_visitation A timeseries object representing the actual visitation that will be plotted along site the visitation_forecast object.
#' @param xlab A string that will be used for the xlabel of the plot
#' @param ylab A string that will be used for the ylabel of the plot
#' @param pred_color a string that will be used for the predicted series color of the plot,
#' @param actual_color a String that will be used for the actual series color of the plot,
#' @param actual_visitation_label a string that will be used for the label of the actual visitation.
#' @param predicted_visitation_label a string that will be used for the label of the actual visitation.
#' @param size A number that represents the thickness of the lines being plotted
#' @param main A string that will be used for the title of the plot
#' @param plot_points a boolean to specify if the plot should be points or continous line.
#' @param date_breaks A string to represent the distance between dates that the x-axis should be in. ex "1 month", "1 year"
#' @param date_labels A string to represent the format of the x-axis time labels.
#' @param ... Additional arguments.
#' @return No return value, called for plotting objects of the class "visitation_forecast".
#'

ggplot.visitation_model <- function(data, mapping = aes(), difference = TRUE, actual_visitation = NULL, predicted_visitation_label = "Predicted", actual_visitation_label = "Actual", xlab = "Time", ylab = "Fitted Value", pred_color = "#ff6361", actual_color = "#ffa600", size = 1.5, main = "Fitted values for visitation model", plot_points = FALSE, date_breaks ="1 year", date_labels = "%y %b", ...)
{
  series_to_plot <- data$visitation_fit

  if(difference)
  {
    series_to_plot <- diff(series_to_plot)
    ylab <- paste(ylab,"(differenced)")
    main <- paste(main, "(differenced)")
  }

  predicted_df_to_plot <- convert_ts_forecast_to_df(series_to_plot)
  predicted_df_to_plot$group <- rep(predicted_visitation_label, length(predicted_df_to_plot$x))

  actual_df_to_plot <- construct_actual_df(actual_visitation, difference, log_outputs = TRUE, actual_visitation_label, plot_cumsum=FALSE, plot_percent_change = FALSE)

  all_labels <- c(predicted_visitation_label, actual_visitation_label)
  data_frame_to_plot<-rbind(predicted_df_to_plot,actual_df_to_plot )
  theme =  ggplot2::theme_gray()+ theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom",
                                        legend.direction = "horizontal", axis.text.x = element_text( size=9), axis.text.y = element_text(size=13))
  scale = scale_x_continuous(breaks = round(seq(min(data_frame_to_plot$x),
                                                max(data_frame_to_plot$x), by = 0.1),1))
  line = geom_line(size = 1.5)
  labels = labs(y= ylab, x = xlab,
                title = main)
  legend = {scale_color_manual(name = "",
                               values = setNames(c(pred_color, actual_color), all_labels))}
  ggplot(data=data_frame_to_plot, aes_string(x ='x', y = 'y',color = 'group'))+ theme + scale + line + labels + legend +scale_x_date(date_breaks = date_breaks,date_labels = date_labels)
}


