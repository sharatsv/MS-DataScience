# Final-project

# Role - Business consultant for Southeast Airlines Co.
# Analyse current data-set to determine:
#  - standing of Southeast Airlines wrt to some KPIs - delay arrival,dept. times, cust. satisfaction etc.
#  - improvement suggestions
#  - prediction of where the Airline would stand after implementing improvements
install.packages("readxl")
library("readxl")
library(gridExtra)
library(ggplot2)

readfromxl <- function(xlpath) {
  return(read_excel(xlpath))
}

get_best_worst_delays <- function(my_data, my_airline) {
  # Summary should show us NAs which we will munge!
  summary(my_data)
  # Get data dim() prior munging
  dim(my_data)
  # Let's get data regarding the total delay in departure
  # and arrival times across each airline-type
  my_data$total_delay_time <- my_data$`Departure Delay in Minutes` + my_data$`Arrival Delay in Minutes`
  airline_names <- unique(my_data$`Airline Name`)
  # > unique(my_data$`Airline Name`)
  # [1] "EnjoyFlying Air Services"         "FlyFast Airways Inc."             "FlyHere Airways"                 
  # [4] "FlyToSun Airlines Inc."           "GoingNorth Airlines Inc."         "West Airways Inc."               
  # [7] "OnlyJets Airlines Inc."           "Northwest Business Airlines Inc." "Oursin Airlines Inc."            
  # [10] "Paul Smith Airlines Inc."         "Sigma Airlines Inc."              "Cheapseats Airlines Inc."        
  # [13] "Southeast Airlines Co."           "Cool&Young Airlines Inc." 
  # 14x Airlines in all!
  mean_delay_time <- character()
  for (name in airline_names) {
    mean_delay_time <- c(mean_delay_time, mean(my_data$total_delay_time[my_data$`Airline Name` == name], 
                                               na.rm = TRUE))
  }
  # Make a data-frame with Airline-name & Mean-delay-time
  dfmeandelaytimes <- data.frame(airline_names, as.numeric(mean_delay_time),
                                 stringsAsFactors = FALSE)
  # Worst airline in terms of delay
  worst_delay_index <- which.max(dfmeandelaytimes$as.numeric.mean_delay_time.)
  cat(dfmeandelaytimes[worst_delay_index, 1],
      "arrives on average", dfmeandelaytimes[worst_delay_index, 2],
      "minutes late\n")
  # Best airline in terms of delay
  best_delay_index <- which.min(dfmeandelaytimes$as.numeric.mean_delay_time.)
  cat(dfmeandelaytimes[best_delay_index, 1],
      "arrives on average", dfmeandelaytimes[best_delay_index, 2],
      "minutes late\n")
  # How Southeast compares to the best & worst
  my_airline_index <- which(dfmeandelaytimes$airline_names == my_airline)
  cat(my_airline, "arrives on average", dfmeandelaytimes[my_airline_index, 2], 
      "minutes late\n")
}

get_popular_carrier <- function(my_data, my_airline){
  # Get a grouping of Airline-names by count (to get popular Airlines)
  my_df <- data.frame(tapply(my_data$`Airline Name`, my_data$`Airline Name`, length))
  my_df$`Airline Names` <- row.names(my_df)
  total <- sum(my_df[,1])
  max_index <- which.max(my_df[,1])
  min_index <- which.min(my_df[,1])
  cat(row.names(my_df[max_index,]), "is the most used airline", "(", 
      as.numeric(my_df[max_index, 1])/total * 100, "% of all bookings)\n")
  cat(row.names(my_df[min_index,]), "is the least used airline", "(", 
      as.numeric(my_df[min_index, 1])/total * 100, "% of all bookings)\n")
  # Sort the data to get position references
  # Determine where my_airline stands in terms of popularity
  sorted_df <- my_df[order(my_df[, 1], decreasing = TRUE),]
  print(sorted_df)
  my_airline_index <- which(sorted_df$`Airline Names` == my_airline)
  cat(row.names(sorted_df[my_airline_index, ]), "is at #", my_airline_index, "(",
                as.numeric(sorted_df[my_airline_index, 1])/total * 100, "% of all bookings)\n")
  return(sorted_df)
}

get_satisfaction_data <- function(my_data, my_airline){
  airline_names <- unique(my_data$`Airline Name`)
  sat_score <- character()
  for (name in airline_names) {
    sat_score <- c(sat_score, mean(my_data$`Satisfaction`[my_data$`Airline Name` == name], 
                                               na.rm = TRUE))
  }
  # Make a data-frame with Airline-name & mean of satisfaction scores
  dfmeansatscores <- data.frame(airline_names, as.numeric(sat_score),
                                 stringsAsFactors = FALSE)
  sorted_df <- dfmeansatscores[order(dfmeansatscores[, 2], 
                                                  decreasing = TRUE), ]
  print(sorted_df)
  max_index = which.max(sorted_df[,2])
  min_index = which.min(sorted_df[,2])
  cat(sorted_df[max_index, 1], "has highest average CSAT(", 
      sorted_df[max_index, 2], " )\n")
  cat(sorted_df[min_index, 1], "has lowest average CSAT(", 
      sorted_df[min_index, 2], " )\n")
  my_airline_index <- which(sorted_df$`airline_names` == my_airline)
  cat(my_airline, "is at #", my_airline_index, "(average CSAT",
      sorted_df[my_airline_index, 2], " )\n")
  # Conclusion at this point is:
  #  - There is no correlation between SAT-score and most-flown Airline
  #  - SouthEast Airlines Co. is at 6th position (off 14) in terms of CSAT
  #  - Figure why passengers are flocking Cheapseats Airlines Inc. (12 off 14) 
  
  # Let's plot histograms for each Airline wrt to Satisfaction survey
  for (name in airline_names) {
    sat_score <- my_data$`Satisfaction`[my_data$`Airline Name` == name]
    hist(sat_score, main=paste("CSAT - ", name), xlab="Satisfaction", col="red")
  }
  return(sorted_df)
}

get_loyalty_membership_data <- function(my_data, my_airline){
  airline_names <- unique(my_data$`Airline Name`)
  loyalty_cards <- character()
  for (name in airline_names) {
    loyalty_cards <- c(loyalty_cards, sum(my_data$`No. of other Loyalty Cards`[my_data$`Airline Name` == name], 
                                   na.rm = TRUE))
  }
  # Make a data-frame with Airline-name & loyalty-cards passengers used
  dfsumloyaltycards <- data.frame(airline_names, as.numeric(loyalty_cards),
                                stringsAsFactors = FALSE)
  sorted_df <- dfsumloyaltycards[order(dfsumloyaltycards[,2], decreasing = TRUE), ]
  print(sorted_df)
  max_index = which.max(sorted_df[,2])
  min_index = which.min(sorted_df[,2])
  cat(sorted_df[max_index, 1], "has highest loyalty-cards offered/used (", 
      sorted_df[max_index, 2], " )\n")
  cat(sorted_df[min_index, 1], "has lowest loyalty-cards offered/used (", 
      sorted_df[min_index, 2], " )\n")
  my_airline_index <- which(sorted_df$`airline_names` == my_airline)
  cat(my_airline, "is at #", my_airline_index, "(loyalty-cards offered/used",
      sorted_df[my_airline_index, 2], " )\n")
  return(sorted_df)
}

# Main
localxlpath <- '/Users/ssharat/Downloads/FinalProjectMaterial/Satisfaction Survey(2).xlsx'
df <- readfromxl(localxlpath)
consulting_airline <- "Southeast Airlines Co."
get_best_worst_delays(df, consulting_airline)
# Get most popular airline
pop_score <- get_popular_carrier(df, consulting_airline)
colnames(pop_score) <- c('Bookings', 'AirlineName')
gg_book <- ggplot(data=pop_score, aes(x=AirlineName, y=Bookings)) 
gg_book <- gg_book + geom_bar(stat="identity", fill='red')
gg_book <- gg_book + coord_flip()
gg_book

# Get data for mean 'Satisfaction' per Airline (sorted)
sat_score <- get_satisfaction_data(df, consulting_airline)
colnames(sat_score) <- c('AirlineName', 'Satisfaction')
gg_sat <- ggplot(data=sat_score, aes(x=AirlineName, y=Satisfaction)) 
gg_sat <- gg_sat + geom_bar(stat="identity", fill='red')
gg_sat <- gg_sat + coord_flip()
gg_sat

# Get loyalty data per Airline
loy_score <- get_loyalty_membership_data(df, consulting_airline)
colnames(loy_score) <- c('AirlineName', 'LoyaltyCards')
gg_loy <- ggplot(data=loy_score, aes(x=AirlineName, y=LoyaltyCards)) 
gg_loy <- gg_loy + geom_bar(stat="identity", fill='red')
gg_loy <- gg_loy + coord_flip()
gg_loy

# Get a combined plot of the above
grid.arrange(gg_book, gg_sat, gg_loy, nrow=2)


# Digging deeper: Using linear regression/correlation analysis to determine
library("gdata")
library(dplyr)
df_recode <- na.omit(df)
df_recode <- df_recode %>% mutate(`Airline Name`=recode(`Airline Name`, 
                                                 "EnjoyFlying Air Services" = 1,
                                                 "FlyFast Airways Inc." = 2, 
                                                 "FlyHere Airways" = 3,
                                                 "FlyToSun Airlines Inc." = 4,
                                                 "GoingNorth Airlines Inc." = 5,
                                                 "West Airways Inc." = 6,
                                                 "OnlyJets Airlines Inc." = 7,
                                                 "Northwest Business Airlines Inc." = 8,
                                                 "Oursin Airlines Inc." = 9,
                                                 "Paul Smith Airlines Inc." = 10,
                                                 "Cheapseats Airlines Inc." = 11,
                                                 "Sigma Airlines Inc." = 12,
                                                 "Southeast Airlines Co." = 13,
                                                 "Cool&Young Airlines Inc." = 14))
df_recode <- df_recode %>% mutate(`Type of Travel`=recode(`Type of Travel`,
                                                   "Business travel" = 1,
                                                   "Personal Travel" = 2,
                                                   "Mileage tickets" = 3))
df_recode <- df_recode %>% mutate(`Airline Status`=recode(`Airline Status`,
                                                          "Platinum" = 1,
                                                          "Gold" = 2,
                                                          "Silver" = 3,
                                                          "Blue" = 4))

# Create a new data-frame with some important columns
df_models <- data.frame(df_recode$`Airline Name`, 
                    df_recode$`Satisfaction`,
                    df_recode$`No. of other Loyalty Cards`,
                    df_recode$`Departure Delay in Minutes`,
                    df_recode$`Arrival Delay in Minutes`,
                    df_recode$`Price Sensitivity`,
                    df_recode$`Type of Travel`,
                    df_recode$`Airline Status`)

colnames(df_models) <- c("Airline Name", 
                         "Satisfaction", 
                         "No. of other Loyalty Cards",
                         "Departure Delay in Minutes", 
                         "Arrival Delay in Minutes",
                         "Price Sensitivity", 
                         "Type of Travel", 
                         "Airline Status")
str(df_models)
lm_model <- lm(formula=`Satisfaction`~`Airline Status`, data=df_models)
summary(lm_model)

lm_model <- lm(formula=`Satisfaction`~`Age`, data=df_models)
summary(lm_model)

lm_model <- lm(formula=`Airline Name`~`No. of other Loyalty Cards`, data=df_models)
summary(lm_model)

plot(df_models$`No. of other Loyalty Cards`, df_models$`Airline Name`, pch=16, col='red',
     ylab="Airline Name",
     xlab="No. of other Loyalty Cards")
# abline(h = 0, v = 0, col = "gray60")
lm_model <- lm(formula=`Airline Name`~., data=df_models)
step(lm_model, data=df_models, direction="backward")
# By running the step function or Parsimonious Model we have:
# Step:  AIC=344145
#`Airline Name` ~ `No. of other Loyalty Cards` + `Departure Delay in Minutes` + 
#  `Arrival Delay in Minutes`
summary(lm_model)

# Predictions using linear model
randindex <- sample(1:dim(df_models)[1])
cutpoint2_3 <- floor(2 * length(randindex) /3)
trainData <- df_models[randindex[1:cutpoint2_3],]
testData <- df_models[randindex[(cutpoint2_3 + 1):length(randindex)],]

lmoutput <- lm(formula=`Airline Name`~., data=trainData)
test <- data.frame(testData$`Airline Name`, 
                   testData$`Satisfaction`,
                   testData$`No. of other Loyalty Cards`,
                   testData$`Departure Delay in Minutes`,
                   testData$`Arrival Delay in Minutes`,
                   testData$`Price Sensitivity`,
                   testData$`Type of Travel`,
                   testData$`Airline Status`
                   )
colnames(test) <- c("Airline Name", "No. of other Loyalty Cards",
                    "Satisfaction",
                    "Departure Delay in Minutes",
                    "Arrival Delay in Minutes",
                    "Price Sensitivity",
                    "Type of Travel",
                    "Airline Status")

lmpredict <- round(predict(lmoutput, test, type="response"))
lm_compTable <- data.frame(testData[,1], lmpredict)
colnames(lm_compTable) <- c('Test', 'Pred')

percentage_lm <- length(which(lm_compTable$Test == lm_compTable$Pred))/dim(lm_compTable)[1]
#11.44%

# Based on AIC:
lmoutput <- lm(formula=`Airline Name`~ `No. of other Loyalty Cards` + 
                 `Departure Delay in Minutes` + 
                 `Arrival Delay in Minutes`, data=trainData)
test <- data.frame(testData$`Airline Name`, 
                   testData$`No. of other Loyalty Cards`,
                   testData$`Departure Delay in Minutes`,
                   testData$`Arrival Delay in Minutes`)
colnames(test) <- c("Airline Name", "No. of other Loyalty Cards",
                    "Departure Delay in Minutes",
                    "Arrival Delay in Minutes")

lmpredict <- round(predict(lmoutput, test, type="response"))
lm_compTable <- data.frame(testData[,1], lmpredict)
colnames(lm_compTable) <- c('Test', 'Pred')

percentage_lm <- length(which(lm_compTable$Test == lm_compTable$Pred))/dim(lm_compTable)[1]

# Calculate the root mean square error(RMSE)
sqrt(mean((lm_compTable$Test - lm_compTable$Pred) ^ 2))
# [1] 3.879554

# Predictions using ksvm
library(kernlab)
cutpoint2_3 <- floor((2 * length(randindex) /3))
trainData <- df_models[randindex[1:cutpoint2_3],]
testData <- df_models[randindex[(cutpoint2_3 + 1):length(randindex)],]

ksvmoutput <- ksvm(`Airline Name`~., data=trainData, 
                   kernel="rbfdot", #kernel function that projects the low dimensional problem into higher dimensional space 
                   kpar="automatic", #params used to control radial function kernel(rbfdot)
                   C=10, #C -> cost of constraints
                   cross=10, #use 10 fold cross-validation in this model
                   prob.model=TRUE)
#test <- data.frame(testData$`Airline Name`, 
#                   testData$`No. of other Loyalty Cards`
#)
#colnames(test) <- c("Airline Name", "No. of other Loyalty Cards")
test <- data.frame(testData$`Airline Name`, 
                   testData$`Satisfaction`,
                   testData$`No. of other Loyalty Cards`,
                   testData$`Departure Delay in Minutes`,
                   testData$`Arrival Delay in Minutes`,
                   testData$`Price Sensitivity`,
                   testData$`Type of Travel`,
                   testData$`Airline Status`
)
colnames(test) <- c("Airline Name", "No. of other Loyalty Cards",
                    "Satisfaction",
                    "Departure Delay in Minutes",
                    "Arrival Delay in Minutes",
                    "Price Sensitivity",
                    "Type of Travel",
                    "Airline Status")

ksvmpredict <- round(predict(ksvmoutput, test, type="response"))
ksvm_compTable <- data.frame(testData[,1], ksvmpredict)
colnames(ksvm_compTable) <- c('Test', 'Pred')

percentage_ksvm <- length(which(ksvm_compTable$Test == ksvm_compTable$Pred))/dim(ksvm_compTable)[1]
percentage_ksvm
# Calculate the root mean square error(RMSE)
sqrt(mean((ksvm_compTable$Test - ksvm_compTable$Pred) ^ 2))


# Predictions using svm
library(e1071)
svmoutput <- svm(`Airline Name`~., data=trainData, 
                 kernel="linear", #kernel function that projects the low dimensional problem into higher dimensional space 
                 cross=10, #use 10 fold cross-validation in this model
                 scale=FALSE)

svmpredict <- round(predict(svmoutput, test, type="response"))
svm_compTable <- data.frame(testData[,1], svmpredict)
colnames(svm_compTable) <- c('Test', 'Pred')

percentage_svm <- length(which(svm_compTable$Test == svm_compTable$Pred))/dim(svm_compTable)[1]


# Making predictions after bumping up the bookings by 10%, 50% and 100%
# Can potentially make a function of this - ran it manually for 10%, 50% and 100%
df_models_order <- df_models[order(df_models$`Airline Name`),]
rownames(df_models_order) <- NULL
df_models_my_airline <- df_models_order[df_models_order$`Airline Name` == 13,]
start_row <- as.numeric(rownames(df_models_my_airline)[1])
end_row <- start_row + nrow(df_models_my_airline) - 1
df_models_order[125867,]
df_models_order[125868,]
for (i in c(start_row:end_row)){
  if (df_models_order[i,3] == 0){
    df_models_order[i,3] <- 1
  } else {
    df_models_order[i,3] <- ceiling(df_models_order[i,3] * 2.0)
  }
}

# Running the KSVM prediction for a small subset since 
# KVSM is compute intensive and taking several hours to give a result
randindex <- sample(1:dim(df_models_order)[1])
cutpoint2_3 <- floor((2 * length(randindex) /3) * 0.1)
trainData <- df_models_order[randindex[1:cutpoint2_3],]
testData <- df_models_order[randindex[(cutpoint2_3 + 1):1000],]

ksvmoutput <- ksvm(`Airline Name`~., data=trainData, 
                   kernel="rbfdot", #kernel function that projects the low dimensional problem into higher dimensional space 
                   kpar="automatic", #params used to control radial function kernel(rbfdot)
                   C=10, #C -> cost of constraints
                   cross=10, #use 10 fold cross-validation in this model
                   prob.model=TRUE)

test <- data.frame(testData$`Airline Name`, 
                   testData$`Satisfaction`,
                   testData$`No. of other Loyalty Cards`,
                   testData$`Departure Delay in Minutes`,
                   testData$`Arrival Delay in Minutes`,
                   testData$`Price Sensitivity`,
                   testData$`Type of Travel`,
                   testData$`Airline Status`
)
colnames(test) <- c("Airline Name", "No. of other Loyalty Cards",
                    "Satisfaction",
                    "Departure Delay in Minutes",
                    "Arrival Delay in Minutes",
                    "Price Sensitivity",
                    "Type of Travel",
                    "Airline Status")

ksvmpredict <- round(predict(ksvmoutput, test, type="response"))
ksvm_compTable <- data.frame(testData[,1], ksvmpredict)
colnames(ksvm_compTable) <- c('Test', 'Pred')
total_my_airline <- count(ksvm_compTable[ksvm_compTable$Pred == 13,])
total_my_airline/nrow(ksvm_compTable) * 100

# Results from above test (also in Project document)
stage_results <- cbind(294, 889, 414)
barplot(stage_results, names.arg = c("10%","50%","100%"), main = "Bookings with increased loyalty cards")

