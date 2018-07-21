############################################
## Master Theses
############################################

# Libraries
Main<-function(){
  while(!require('data.table'))
    install.packages('data.table')
  while(!require('ggplot2'))
    install.packages('ggplot2')
  while(!require('dplyr'))
    install.packages('dplyr')
  while(!require('emil'))
    install.packages('emil')
  while(!require('magrittr'))
    install.packages('magrittr')
  while(!require('purrrlyr'))
    install.packages('purrrlyr')
  while(!require('MASS'))
    install.packages('MASS')
  while(!require('sjlabelled'))
    install.packages('sjlabelled')
  while(!require('tidyr'))
    install.packages('tidyr')
  while(!require('xgboost'))
    install.packages('xgboost')
  while(!require('caret'))
    install.packages('caret')
  while(!require('gbm'))
    install.packages('gbm')
 
  # Set switches 
  loadingSwitch <- TRUE
  now<-Sys.time()
  loading(loadingSwitch)
  print(Sys.time()-now)
  # preproc function requires the switch, the dataframe you wish to process and the name of dataframe you wish to store the processed dataframe in
  # input_test is the dataframe you whish to preproces
  # name_new_dataframe is the name of the new dataframe
  
}

############################################
## Loading dataset for each user
############################################

loading<-function(loadingSwitch){
  if(loadingSwitch)
  {
    data.folder  <<- 
      if(grepl("Emma", getwd())) {
        "/Users/Emma/Documents/Master/Scriptie/Data/finaldatacsv.csv"
      }
    
    data <- fread(data.folder, header=TRUE, na.strings=c("","NULL","NA"))       # 31 seconde bij Emma, 6 bij Roel
    colnames(data) = gsub(" ", "_",colnames(data))
    data$Created_date<- gsub("T", " ", data$Created_date)
    data$Created_date<- gsub("Z", "", data$Created_date)
    data$Updated_date<- gsub("T", " ", data$Updated_date)
    data$Updated_date<- gsub("Z", "", data$Updated_date)
    
    data$date <- substr(data$Created_date, 1, 10)
    data$date <- as.Date(data$date)
    data$datetime <- as.POSIXct(data$Created_date)
    data$datetimefinish <- as.POSIXct(data$Updated_date)
    
    testfull <- data[data$date >= "2018-06-21" & data$date <= "2018-06-22",]
    mainfull <-data[data$date >= "2018-06-22" & data$date <= "2018-07-21",]
    
    testfull$Title <- NULL
    testfull$Owner <- NULL
    mainfull$Title <- NULL
    mainfull$Owner <- NULL
    test <- na.omit(testfull)
    main <- na.omit(mainfull)
    
  }
}

