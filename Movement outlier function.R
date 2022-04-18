Anomaly_detect=function(data,confidence=.995, vertical=1, ID){
  ###Assumes movebank naming conventions for column names
  ###Assumes coordinates are in long lat
  ###vertical argument: if 1 (default) height information not included, if 2 height included, if 3 height and vertical speed included
  ###ID is the column number for individual animal IDs
  ###confidence argument is the probability threshold for an outlier. Must be numeric between 0 and 1, default = .995. I recommend values at our above the default. 
  
  #Check that required packages are installed on the system
  areinstaled=data.frame(installed.packages())
  
  if(all(c("ctmm","lubridate","solitude","forcats")%in%areinstaled$Package)==FALSE){
    required_packages=c("ctmm","lubridate","solitude","forcats")
    missing_packages=c("ctmm","lubridate","solitude","forcats")%in%areinstaled$Package
    stop(paste("The following packages are not installed:", required_packages[which(missing_packages==FALSE)], sep = " "))
  }
  
  ##load required packages if not already loaded
  suppressMessages(require("ctmm"))
  suppressMessages(require("forcats"))
  suppressMessages(require("lubridate"))
  suppressMessages(require("solitude"))
  
  
  data=data
  if(exists("ID")==FALSE){
    stop("ID column information missing. Please provide the column number with individual IDs")
  }
  
if(is.data.frame(data)==FALSE){
  stop("Data must be a data frame")
}
  
  if(lubridate::is.POSIXct(data$timestamp)==FALSE){
    stop("Timestamps must be a POSIXct object")
  }

  
  #utilize ctmm to estimate speed and distance metrics that are tailored for outlier detection and that account for telemetry error and timestamp truncation
  message("Estimating speed and distance components")
  temp=suppressMessages(ctmm::as.telemetry(data))
  temp_out=suppressMessages(ctmm::outlie(temp, plot = FALSE))

  data$speeds=temp_out$speed
  data$distance=temp_out$distance
  
  
  if(vertical == 2 | vertical == 3){
    if('vertical.speed' %in% names(temp_out)==FALSE){
      warning("Vertical information not available in data, proceeding without vertical component")
      vertical = 1
    }else{
      data$height=temp_out$vertical.distance
      data$vert_speed=temp_out$vertical.speed
    }

  } 
  
  ##check if default isolation forest sample size settings need to be adjusted
  if(nrow(data)<256){
    sample_size=nrow(data)
  }else{
    sample_size=256
  }
  
  #Begin anomaly detection
  message("Initiating anomaly detection")
  if(all(diff((data$timestamp)) > 0)==FALSE){
    stop("Timestamps may be out of order, please ensure that timestamps are in ascending order")
  }
  
  if(vertical == 3){
    indexs=which(complete.cases(data$height,data$vert_speed,data$speeds,data$distance)==TRUE)
    COLS=c("height","vert_speed","speeds","distance")
    COLS=which(names(data)%in%COLS==TRUE)
    subset=data[indexs,COLS]
    iforest = isolationForest$new(sample_size=sample_size)
    invisible(capture.output(iforest$fit(subset)))
    scores =iforest$predict(subset)
    data$overall_anomaly_score=NA
    data$overall_anomaly_score[indexs]=scores$anomaly_score
    
    indexs_HAE=which(complete.cases(data$height,data$vert_speed)==TRUE)
    COLS_HAE=c("height","vert_speed")
    COLS_HAE=which(names(data)%in%COLS_HAE==TRUE)
    subset_HAE=data[indexs_HAE,COLS_HAE]
    iforest_HAE = isolationForest$new(sample_size=sample_size)
    invisible(capture.output(iforest_HAE$fit(subset_HAE)))
    scores_HAE =iforest_HAE$predict(subset_HAE)
    data$anomaly_score_vertical=NA
    data$anomaly_score_vertical[indexs_HAE]=scores_HAE$anomaly_score
    
  }
  
  if(vertical == 2){
    indexs=which(complete.cases(data$height,data$speeds,data$distance)==TRUE)
    COLS=c("height","speeds","distance")
    COLS=which(names(data)%in%COLS==TRUE)
    subset=data[indexs,COLS]
    iforest = isolationForest$new(sample_size=sample_size)
    invisible(capture.output(iforest$fit(subset)))
    scores =iforest$predict(subset)
    data$overall_anomaly_score=NA
    data$overall_anomaly_score[indexs]=scores$anomaly_score
    
    indexs_HAE=which(complete.cases(data$height)==TRUE)
    COLS_HAE=c("height")
    COLS_HAE=which(names(data)%in%COLS==TRUE)
    subset_HAE=data[indexs_HAE,COLS_HAE]
    iforest_HAE = isolationForest$new(sample_size=sample_size)
    invisible(capture.output(iforest_HAE$fit(subset_HAE)))
    scores_HAE =iforest_HAE$predict(subset_HAE)
    data$anomaly_score_vertical=NA
    data$anomaly_score_vertical[indexs_HAE]=scores_HAE$anomaly_score
    
  }
  
  if(vertical == 1){
    indexs=which(complete.cases(data$speeds,data$distance)==TRUE)
    COLS=c("speeds","distance")
    COLS=which(names(data)%in%COLS==TRUE)
    subset=data[indexs,COLS]
    iforest = isolationForest$new(sample_size=sample_size)
    invisible(capture.output(iforest$fit(subset)))
    scores =iforest$predict(subset)
    data$overall_anomaly_score=NA
    data$overall_anomaly_score[indexs]=scores$anomaly_score
    
  }
  
  
  indexs_DFH=which(complete.cases(data$distance)==TRUE)
  subset_DFH=as.data.frame(data$distance[indexs_DFH])
  iforest_DFH = isolationForest$new(sample_size=sample_size)
  invisible(capture.output(iforest_DFH$fit(subset_DFH)))
  scores_DFH =iforest_DFH$predict(subset_DFH)
  data$anomaly_score_distance=NA
  data$anomaly_score_distance[indexs_DFH]=scores_DFH$anomaly_score
  
  
  
  indexs_speed=which(complete.cases(data$speeds)==TRUE)
  subset_speed=as.data.frame(data$speeds[indexs_speed])
  iforest_speed = isolationForest$new(sample_size=sample_size)
  invisible(capture.output(iforest_speed$fit(subset_speed)))
  scores_speed =iforest_speed$predict(subset_speed)
  data$anomaly_score_speed=NA
  data$anomaly_score_speed[indexs_speed]=scores_speed$anomaly_score
  
  overall_outliers=which(data$overall_anomaly_score>=as.numeric(quantile(na.omit(data$overall_anomaly_score), confidence)))
  speed_outliers=which(data$anomaly_score_speed>=as.numeric(quantile(na.omit(data$anomaly_score_speed), confidence)))
  distance_outliers=which(data$anomaly_score_distance>=as.numeric(quantile(na.omit(data$anomaly_score_distance), confidence)))
  
  distance_outliers=distance_outliers[-which(distance_outliers%in%overall_outliers==TRUE)]
  speed_outliers=speed_outliers[-which(speed_outliers%in%overall_outliers==TRUE)]
  
  data$Outliers="Not anomalous"
  data$Outliers[speed_outliers]="Possibly anomalous speed"
  data$Outliers[distance_outliers]="Possibly anomalous distance"
  
  if(vertical == 2 | vertical == 3){
    vertical_outliers=which(data$anomaly_score_vertical>=as.numeric(quantile(na.omit(data$anomaly_score_vertical), confidence)))
    vertical_outliers=vertical_outliers[-which(vertical_outliers%in%overall_outliers==TRUE)]
    data$Outliers[vertical_outliers]="Possibly vertical anomaly"
    
  }
  
  data$Outliers[overall_outliers]="Unanimous outlier"
  
  
  data$Outliers_plot_lable <- ifelse(data$Outliers == "Not anomalous", NA_character_, data$Outliers)
  
  if(vertical == 2 | vertical == 3){
    data$Outliers_plot_lable <- fct_relevel(data$Outliers_plot_lable, "Unanimous outlier", "Possibly anomalous speed","Possibly anomalous distance" ,"Possibly vertical anomaly")
  }
  
  if(vertical == 1){
    data$Outliers_plot_lable <- fct_relevel(data$Outliers_plot_lable, "Unanimous outlier", "Possibly anomalous speed","Possibly anomalous distance")
  }
  
  numfixes=nrow(data)
  total_negative=length(data$Outliers[which(data$Outliers=="No")])
  True_positive=length(which(data$Outliers=="Unanimous outlier"))
  True_negative=length(which(data$Outliers=="Not anomalous"))
  Soft_positive=length(which(data$Outliers!="Unanimous outlier"& data$Outliers!="Not anomalous"))
  
  sumanom=paste("Of", numfixes, "fixes,", True_positive, "were identified as unanimously anomalous,", Soft_positive, "were identified as one dimensional anomalies, and", True_negative, "were not anomalous", sep=" ")
  message(sumanom)
  assign("Outlier_data",data, envir = parent.frame())
  message("Data annotated with outlier information, stored in global environment as Outlier_data")  
  
  invisible(data)
  
}


plot_anomaly=function(data, coords){
  ###Create interactive plot of outliers
  ###data must be a dataframe
  #coords must be a vector of column numbers for longitude and latitude respectively
  data=data.frame(data)
  coords=coords
  areinstaled=data.frame(installed.packages())
  
  if(all(c("sp","rgdal","ggplot2","plotly")%in%areinstaled$Package)==FALSE){
    required_packages=c("sp","rgdal","ggplot2","plotly")
    missing_packages=c("sp","rgdal","ggplot2","plotly")%in%areinstaled$Package
    stop(paste("The following packages are not installed:", required_packages[which(missing_packages==FALSE)], sep = " "))
  }
  suppressMessages(require("sp"))
  suppressMessages(require("rgdal"))
  suppressMessages(require("ggplot2"))
  suppressMessages(require("plotly"))
  
  data$INDEX=row(data)[,1]
  
  data2 <- SpatialPointsDataFrame(coords = data[,c(coords[1],coords[2])], data = data,
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))
  data2 <- spTransform(data2, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs"))
  data=as.data.frame(data2)
  
  
  
  if("height"%in%names(data)==TRUE){
    colnames(data)[c(ncol(data)-1,ncol(data))]=c("X","Y")
    COLS=c("INDEX","height","distance","speeds","overall_anomaly_score", "anomaly_score_vertical","anomaly_score_distance", "anomaly_score_speed", "Outliers", "Outliers_plot_lable")
    COLS=which(names(data)%in%COLS==TRUE)
    colnames(data)[COLS]=c("Speed","Distance","Height","Overall Anomaly Score", "Vertical anomaly score","Distance anomaly score", "Speed anomaly score", "Outlier detection","Outlier assessment","Row number")
    Anomalies=ggplot(data,
                     aes(x=X, 
                         y=Y,
                         label = `Row number`, 
                         label2 = `Overall Anomaly Score`,
                         label3 = `Distance anomaly score`,
                         label4 = `Speed anomaly score`,
                         label5 = `Vertical anomaly score`,
                         label6 = Distance, 
                         label7 = Speed, 
                         label8 = Height))+ 
      geom_path(aes(group=1),
                size = 0.3,
                alpha = 0.5,
                color = "grey30")+
      geom_point(aes(color= `Outlier assessment`),
                 alpha = 0.8)+
      ggtitle("Outliers")+
      xlab("X")+
      ylab("Y")+
      scale_color_manual(name = "Outlier detection:",
                         values = c("red", "chartreuse4","darkorange2" ,"darkgoldenrod1"),
                         na.translate = FALSE)+
      theme_classic()+
      theme(legend.position="top")+
      guides(color = guide_legend(override.aes = list(alpha = 1,
                                                      size = 2) ) ) +
      coord_equal(ratio = 1)
    Anomalies
    p1=ggplotly(Anomalies)
    p1$x$data[[length(p1$x$data)]]$name = " "
    return(p1)
  }else{
    colnames(data)[c(ncol(data)-1,ncol(data))]=c("X","Y")
    COLS=c("INDEX","distance","speeds","overall_anomaly_score", "anomaly_score_distance", "anomaly_score_speed", "Outliers", "Outliers_plot_lable")
    COLS=which(names(data)%in%COLS==TRUE)
    colnames(data)[COLS]=c("Speed","Distance","Overall Anomaly Score","Distance anomaly score", "Speed anomaly score", "Outlier detection","Outlier assessment","Row number")
    Anomalies=ggplot(data,
                     aes(x=X, 
                         y=Y,
                         label = `Row number`, 
                         label2 = `Overall Anomaly Score`,
                         label3 = `Distance anomaly score`,
                         label4 = `Speed anomaly score`,
                         label5 = Distance, 
                         label6 = Speed))+ 
      geom_path(aes(group=1),
                size = 0.3,
                alpha = 0.5,
                color = "grey30")+
      geom_point(aes(color= `Outlier assessment`),
                 alpha = 0.8)+
      ggtitle("Outliers")+
      xlab("X")+
      ylab("Y")+
      scale_color_manual(name = "Outlier detection:",
                         values = c("red", "chartreuse4","darkorange2" ),
                         na.translate = FALSE)+
      theme_classic()+
      theme(legend.position="top")+
      guides(color = guide_legend(override.aes = list(alpha = 1,
                                                      size = 2) ) ) +
      coord_equal(ratio = 1)
    Anomalies
    p1=ggplotly(Anomalies)
    p1$x$data[[length(p1$x$data)]]$name = " "
    return(p1)
    
  }
  
}



zoom_anomaly=function(data,coords, Outlier ){
  ###Create an interactive plot zoomed into a particular outlier of interest
  ###data must be a dataframe
  #coords must be a vector of column numbers for longitude and latitude respectively
  #Outlier argument must be the rownumber of the focal outlier
  data=data.frame(data)
  coords=coords
  areinstaled=data.frame(installed.packages())
  if(Outlier<=5){
    Index=seq(from=Outlier+((0-Outlier)+1), to=Outlier+5, by=1)
  }else{
    Index=seq(from=Outlier-5, to=Outlier+5, by=1)
  }
  if(all(c("sp","rgdal","ggplot2","plotly")%in%areinstaled$Package)==FALSE){
    required_packages=c("sp","rgdal","ggplot2","plotly")
    missing_packages=c("sp","rgdal","ggplot2","plotly")%in%areinstaled$Package
    stop(paste("The following packages are not installed:", required_packages[which(missing_packages==FALSE)], sep = " "))
  }
  suppressMessages(require("sp"))
  suppressMessages(require("rgdal"))
  suppressMessages(require("ggplot2"))
  suppressMessages(require("plotly"))
  
  data$INDEX=row(data)[,1]
  
  data2 <- SpatialPointsDataFrame(coords = data[,c(coords[1],coords[2])], data = data,
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))
  data2 <- spTransform(data2, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs"))
  data=as.data.frame(data2)
  
  if("height"%in%names(data)==TRUE){
    colnames(data)[c(ncol(data)-1,ncol(data))]=c("X","Y")
    COLS=c("INDEX","distance","speeds","overall_anomaly_score", "anomaly_score_distance", "anomaly_score_speed", "Outliers", "Outliers_plot_lable")
    COLS=which(names(data)%in%COLS==TRUE)
    colnames(data)[COLS]=c("Speed","Distance","Overall Anomaly Score","Distance anomaly score", "Speed anomaly score", "Outlier detection","Outlier assessment","Row number")
    Anomalies=ggplot(data[Index,],
                     aes(x=X, 
                         y=Y,
                         label = `Row number`, 
                         label2 = `Overall Anomaly Score`,
                         label3 = `Distance anomaly score`,
                         label4 = `Speed anomaly score`,
                         label5 = `Vertical anomaly score`,
                         label6 = Distance, 
                         label7 = Speed, 
                         label7 = Height))+ 
      geom_path(aes(group=1),
                size = 0.3,
                alpha = 0.5,
                color = "grey30")+
      geom_point(aes(color= `Outlier assessment`),
                 alpha = 0.8)+
      ggtitle("Outliers")+
      xlab("X")+
      ylab("Y")+
      scale_color_manual(name = "Outlier detection:",
                         values = c("red", "chartreuse4","darkorange2" ,"darkgoldenrod1"),
                         na.translate = FALSE)+
      theme_classic()+
      theme(legend.position="top")+
      guides(color = guide_legend(override.aes = list(alpha = 1,
                                                      size = 2) ) ) +
      coord_equal(ratio = 1)
    Anomalies
    p1=ggplotly(Anomalies)
    p1$x$data[[length(p1$x$data)]]$name = " "
    return(p1)
  }else{
    colnames(data)[c(ncol(data)-1,ncol(data))]=c("X","Y")
    COLS=c("INDEX","distance","speeds","overall_anomaly_score", "anomaly_score_distance", "anomaly_score_speed", "Outliers", "Outliers_plot_lable")
    COLS=which(names(data)%in%COLS==TRUE)
    colnames(data)[COLS]=c("Speed","Distance","Overall Anomaly Score", "Distance anomaly score", "Speed anomaly score", "Outlier detection","Outlier assessment","Row number")
    Anomalies=ggplot(data[Index,],
                     aes(x=X, 
                         y=Y,
                         label = `Row number`, 
                         label2 = `Overall Anomaly Score`,
                         label3 = `Distance anomaly score`,
                         label4 = `Speed anomaly score`,
                         label5 = Distance, 
                         label6 = Speed))+ 
      geom_path(aes(group=1),
                size = 0.3,
                alpha = 0.5,
                color = "grey30")+
      geom_point(aes(color= `Outlier assessment`),
                 alpha = 0.8)+
      ggtitle("Outliers")+
      xlab("X")+
      ylab("Y")+
      scale_color_manual(name = "Outlier detection:",
                         values = c("red", "chartreuse4","darkorange2" ),
                         na.translate = FALSE)+
      theme_classic()+
      theme(legend.position="top")+
      guides(color = guide_legend(override.aes = list(alpha = 1,
                                                      size = 2) ) ) +
      coord_equal(ratio = 1)
    Anomalies
    p1=ggplotly(Anomalies)
    p1$x$data[[length(p1$x$data)]]$name = " "
    return(p1)
    
  }
  
}
 
