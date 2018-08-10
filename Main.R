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
 
  install.packages("stargazer")
  library(stargazer)
  
  install.packages("xtable")
  library(xtable)
  
  install.packages("ggpubr")
  library("ggpubr")
  
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
  
    #fred geert emma eruit
    main <- main[-c(4, 67, 144), ]
    #caroline en janwillem
    main[67, 3] = "Male"
    main[34, 3] = "Female"    
    # order by gender
    test %>%
      group_by(Gender) %>%
      summarise(n_distinct(ID))
    
    # average duration 
    (sum(test$datetimefinish - test$datetime) - 269)/28
    
    main %>%
      group_by(Gender) %>%
      summarise(n_distinct(ID))
    
    ((sum(main$datetimefinish - main$datetime))-309-585-102-119-244)/136
    
    
    #histograms
    histo <- test %>% 
      group_by(Age) %>%
      summarise(number = n())
    
    ggplot(data=histo, aes(x=Age, y=number)) +
      geom_bar(stat="identity") + scale_x_continuous("Age") + theme(axis.text.x = element_text( hjust = 1))
    
    
    
     histo <- main %>% 
      group_by(Age) %>%
      summarise(number = n())
    
    ggplot(data=histo, aes(x=Age, y=number)) +
      geom_bar(stat="identity") + scale_x_continuous("Age") + theme(axis.text.x = element_text( hjust = 1))
    
    histo <- main %>% 
      group_by(Leaderbord, Gender) %>%
      summarise(number = n())
    
    #to latex table
    object <- aggregate(cbind(Week, Week2, Week3) ~ Leaderbord + Gender, data = test, FUN = function(x) c(med = median(x),mean = mean(x)) )
    object1 <-as.data.frame(lapply(object, unlist))
    xtable(object1)

    object <- aggregate(cbind(Week, Week2, Week3) ~ Leaderbord , data = test, FUN = function(x) c(med = median(x),mean = mean(x), n = length(x) ) )
    object <- aggregate(cbind(Week, Week2, Week3) ~ Gender , data = test, FUN = function(x) c(med = median(x),mean = mean(x) ) )
    
    
   object <-  aggregate(cbind(Week, Week2, Week3) ~ Leaderbord + Gender, data = main, FUN = function(x) c(med = median(x), mean = mean(x)) )
    object <- aggregate(cbind(Week, Week2, Week3) ~ Leaderbord , data = main, FUN = function(x) c(med = median(x), mean = mean(x) ) )
    object <- aggregate(cbind(Week, Week2, Week3) ~ Gender , data = main, FUN = function(x) c(med = median(x), mean = mean(x) ) )
    
    
    # histograms leaderbord and no
    histo <- main %>% 
      group_by(Leaderbord, Week) %>%
      summarise(number = n())
    histo$count <- as.integer(histo$Leaderbord) * 68
    histo$count<- gsub(0, 73, histo$count)
    histo$percentage <- histo$number/as.integer(histo$count)
    
    ggplot(histo,aes(x=Week,y=percentage,fill=factor(Leaderbord)))+
      geom_bar(stat="identity",position="dodge")+
      scale_fill_discrete(name="Leaderbord",
                          breaks=c(0, 1),
                          labels=c("No", "Yes"))+
      xlab("Weeks left")+ylab("Percentage participants booked") +theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    histo <- main %>% 
      group_by(Leaderbord, Week2) %>%
      summarise(number = n())
    histo$count <- as.integer(histo$Leaderbord) * 68
    histo$count<- gsub(0, 73, histo$count)
    histo$percentage <- histo$number/as.integer(histo$count)
    
    ggplot(histo,aes(x=Week2,y=percentage,fill=factor(Leaderbord)))+
      geom_bar(stat="identity",position="dodge")+
      scale_fill_discrete(name="Leaderbord",
                          breaks=c(0, 1),
                          labels=c("No", "Yes"))+
      xlab("Weeks left")+ylab("Percentage participants booked") +theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    histo <- main %>% 
      group_by(Leaderbord, Week3) %>%
      summarise(number = n())
    histo$count <- as.integer(histo$Leaderbord) * 68
    histo$count<- gsub(0, 73, histo$count)
    histo$percentage <- histo$number/as.integer(histo$count)
    
    ggplot(histo,aes(x=Week3,y=percentage,fill=factor(Leaderbord)))+
      geom_bar(stat="identity",position="dodge")+
      scale_fill_discrete(name="Leaderbord",
                          breaks=c(0, 1),
                          labels=c("No", "Yes"))+
      xlab("Weeks left")+ylab("Percentage participants booked") +theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
    
    # Tests
    #wilcox
    
    data1 <- main[,c("Leaderbord","Week")]
    group_by(data1, Leaderbord) %>%
      summarise(
        count = n(),
        median = median(Week, na.rm = TRUE),
        IQR = IQR(Week, na.rm = TRUE)
      )
    ggboxplot(data1, x = "Leaderbord", y = "Week", 
              color = "Leaderbord", palette = c("#00AFBB", "#E7B800"),
              ylab = "Week", xlab = "Leaderbord")
    
    # GAME 1
    # two-sided
    wilcox.test(Week ~ Leaderbord, data = main,
                exact = FALSE)
    # one sided
    wilcox.test(Week ~ Leaderbord, data = main,
                exact = FALSE, alternative = "greater")
    
    # GAME 2
    # two-sided
    wilcox.test(Week2 ~ Leaderbord, data = main,
                exact = FALSE)
    # one sided
    wilcox.test(Week2 ~ Leaderbord, data = main,
                exact = FALSE, alternative = "greater")
    
    # GAME 3
    # two-sided
    wilcox.test(Week3 ~ Leaderbord, data = main,
                exact = FALSE)
    # one sided
    wilcox.test(Week3 ~ Leaderbord, data = main,
                exact = FALSE, alternative = "greater")
    
    
    
    # chi-square
    chisq.test(table(main$Week, main$Leaderbord))
    
    chisq.test(table(main$Week2, main$Leaderbord))
    
    chisq.test(table(main$Leaderbord, main$Week3))
    
    # t-test
    t.test(Week ~ Leaderbord, data = main)
    # one sided

    
    # GAME 2
    # two-sided
    t.test(Week2 ~ Leaderbord, data = main)
    # one sided

    
    # GAME 3
    # two-sided
    t.test(Week3 ~ Leaderbord, data = main)
    
    # game 2 and 3
    
    data2 <- main[,c("Week2","Week3")]
   
    group2 <- group_by(data2, Week2) %>%
      summarise(
        count = n()
      )
    group3 <- group_by(data2, Week3) %>%
      summarise(
        count = n()
      )
    wilcox.test(main$Week2, mu = median(main$Week3))
    wilcox.test(main$Week3, mu = median(main$Week2))
    
    
    
    
    week2 <- c(39,0,0,0,0,0,0,1,0,0,3,2,4,79,2,11)
    week3 <- c(99,1,1,2,1,0,0,16,1,1,3,3,2,2,1,8)
    survey = as.data.frame(rbind(week2, week3))
    names(survey) = c('0', '1', '2','3', '4', '5','6', '7', '8','9', '10', '11','12', '13', '14', '15')
    chisq.test(survey)
  }
}


game <- main$Week2
week <- main$Week2
hyp2 <- data.frame(game, week)
hyp2$game <- "2"

game <- main$Week3
week <- main$Week3
hyp3 <- data.frame(game, week)
hyp3$game <- "3"

hyp <- rbind(hyp2, hyp3)

# GAME 1
# two-sided
wilcox.test(week ~ game, data = hyp,
            exact = FALSE)
# one sided
wilcox.test(week ~ game, data = hyp,
            exact = FALSE, alternative = "greater")


# t-test
t.test(week ~ game, data = hyp)
# one sided


# GAME 2
# two-sided
t.test(Week2 ~ Leaderbord, data = main)
# one sided


# GAME 3
# two-sided
t.test(Week3 ~ Leaderbord, data = main)