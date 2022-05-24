#
#      Course: IST-687
#      Name: Sharat Sripada
#      Homework #9
#      Due Date: 3/15/2020
#      Date Submitted: 3/15/2020
#      Topic: SVMs, Comparing different models - Classification, Regression.
#
# install.packages("kernlab")
# install.packages("gridExtra")
# For KSVM
library(kernlab)
# For SVM
library(e1071)
# For plottting multiple graphs in one
library(gridExtra)

aq <- data.frame(airquality)

# Replace NAs with mean
ozone_mean <- mean(na.omit(aq$Ozone))
solar_mean <- mean(na.omit(aq$Solar.R))
aq$Ozone[is.na(aq$Ozone)] <- ozone_mean
aq$Solar.R[is.na(aq$Solar.R)] <- solar_mean

dim(aq)
randindex <- sample(1:dim(aq)[1])

# By theory, we use 2/3rd data for trainData & 1/3rd 
# data for testData.
cutpoint2_3 <- floor(2 * length(randindex) /3)
trainData <- aq[randindex[1:cutpoint2_3],]
testData <- aq[randindex[(cutpoint2_3 + 1):length(randindex)],]

# Build a model using kernel SVM 
ksvmoutput <- ksvm(Ozone~., data=trainData, 
                  kernel="rbfdot", #kernel function that projects the low dimensional problem into higher dimensional space 
                  kpar="automatic", #params used to control radial function kernel(rbfdot)
                  C=10, #C -> cost of constraints
                  cross=10, #use 10 fold cross-validation in this model
                  prob.model=TRUE)
ksvmoutput

# Predict data based on data from the model/svmoutput
# & testData
ksvmpredict <- predict(ksvmoutput, testData, type="votes")
str(ksvmpredict)
str(testData)

# Create a comparison data-frame that contains the testData for Ozone
# & predicted values using the ksvm() function
compTable <- data.frame(testData[,1], ksvmpredict[,1])
colnames(compTable) <- c('Test', 'Pred')
compTable

# Calculate the root mean square error(RMSE)
sqrt(mean((compTable$Test - compTable$Pred) ^ 2))
# RMSE=17.72

# Compute absolute error
compTable$error <- abs(compTable$Test - compTable$Pred)

# Create a new data-frame with error, temp, wind data
ksvmPlot <- data.frame(compTable$error, testData$Temp, testData$Wind)

# Assign column names
colnames(ksvmPlot) <- c('Abs.Error', 'Temp', 'Wind')

# Plot the data-frame using ggplot
library(ggplot2)
ksvm_ggplot <- ggplot(ksvmPlot, aes(x=Temp, y=Wind)) + geom_point(aes(size=Abs.Error, color=Abs.Error)) +
  ggtitle("ksvm")

ksvm_ggplot

# Build a model using SVM
svmoutput <- svm(Ozone~., data=trainData, kernel="linear", cost=10, scale=FALSE)

svmoutput

# Predict data based on data from the model/svmoutput
# & testData
svmpredict <- predict(svmoutput, testData, type="votes")
str(svmpredict)
str(testData)

# Create a comparison data-frame that contains the testData for Ozone
# & predicted values using the ksvm() function
svm_compTable <- data.frame(testData[,1], svmpredict)
colnames(svm_compTable) <- c('Test', 'Pred')
svm_compTable

# Calculate the root mean square error(RMSE)
sqrt(mean((svm_compTable$Test - svm_compTable$Pred) ^ 2))
# RMSE=19.47

# Compute absolute error
svm_compTable$error <- abs(svm_compTable$Test - svm_compTable$Pred)

# Create a new data-frame with error, temp, wind data
svmPlot <- data.frame(svm_compTable$error, testData$Temp, testData$Wind)

# Assign column names
colnames(svmPlot) <- c('Abs.Error', 'Temp', 'Wind')

# Plot the data-frame using ggplot
svm_ggplot <- ggplot(svmPlot, aes(x=Temp, y=Wind)) + geom_point(aes(size=Abs.Error, color=Abs.Error)) +
  ggtitle("svm")

svm_ggplot

# Build a model using liner regression (lm function)
lmoutput <- lm(formula=Ozone~., data=testData)
lm_test <- data.frame(Solar.R=aq$Solar.R, Wind=aq$Wind, 
                   Temp=aq$Temp, Month=aq$Month, Day=aq$Day)

lmpredict <- predict(lmoutput, lm_test, type="response")

# Create a comparison data-frame that contains the testData for Ozone
# & predicted values using the lm() function
lm_compTable <- data.frame(testData[,1], lmpredict)
colnames(lm_compTable) <- c('Test', 'Pred')

# Calculate the root mean square error(RMSE)
sqrt(mean((lm_compTable$Test - lm_compTable$Pred) ^ 2))
# RMSE=29.68

# Compute absolute error
lm_compTable$error <- abs(lm_compTable$Test - lm_compTable$Pred)

# Create a new data-frame with error, temp, wind data
lmPlot <- data.frame(lm_compTable$error, testData$Temp, testData$Wind)

# Assign column names
colnames(lmPlot) <- c('Abs.Error', 'Temp', 'Wind')

# Plot the data-frame using ggplot
lm_ggplot <- ggplot(lmPlot, aes(x=Temp, y=Wind)) + geom_point(aes(size=Abs.Error, color=Abs.Error)) +
  ggtitle("lm")

lm_ggplot

# Conclusion:
#  - RMSE for ksvm(17.72) is lower than RMSE for svm(19.47) & lm(29.68)
#  - Plotting the abs. error also showed a higher range & number for lm model (kvm and svm are comparable)
# For the given data-set, KSVM is a marginally better algorithm than svm & way better than lm

# Using gridExtra to represent graphs in one plane
grid.arrange(ksvm_ggplot, svm_ggplot, lm_ggplot, nrow=2)

# Moving now to classification based algorithms.
#  - classification based algorithms predict with 0/1
#  - regression/linear based algorithms (previous section) predict a value

# Creating a new var goodOzone: if Ozone >= meanOzone then 1 else 0
trainData$goodOzone <- ifelse(trainData$Ozone < ozone_mean, 0, 1)
testData$goodOzone <- ifelse(testData$Ozone < ozone_mean, 0, 1)

# Remove Ozone from trainData & testData
trainData <- trainData[,-1]
testData <- testData[,-1]
trainData$goodOzone <- as.factor(trainData$goodOzone)
testData$goodOzone <- as.factor(testData$goodOzone)

# Build a model based on ksvm
ksvmgood <- ksvm(goodOzone~., data=trainData, 
                   kernel="rbfdot", #kernel function that projects the low dimensional problem into higher dimensional space 
                   kpar="automatic", #params used to control radial function kernel(rbfdot)
                   C=10, #C -> cost of constraints
                   cross=10, #use 10 fold cross-validation in this model
                   prob.model=TRUE)
ksvmgood

# Predict data based on data from the model/svmoutput
# & testData
ksvm_goodPred <- predict(ksvmgood, testData)
ksvm_goodPred # This should yield a 0/1

# Create a comparison data-frame that contains the testData for Ozone
# & predicted values using the ksvm() function
ksvm_goodcompTable <- data.frame(testData[,6], ksvm_goodPred)
colnames(ksvm_goodcompTable) <- c('Test', 'Pred')
ksvm_goodcompTable

# Calculate the percentage of correct values (this is different from the 
# linear/regression models where we calculate RMSE)
percentage_ksvm <- length(which(ksvm_goodcompTable$Test == ksvm_goodcompTable$Pred))/dim(ksvm_goodcompTable)[1]
percentage_ksvm
# Pecentage = 0.6862

# Confusion matrix
results <- table(Test=ksvm_goodcompTable$Test, Pred=ksvm_goodcompTable$Pred)
print(results)

# Plot the results
ksvm_goodcompTable$correct <- ifelse(ksvm_goodcompTable$Test==ksvm_goodcompTable$Pred,"correct","wrong")
plot_ksvm <- data.frame(ksvm_goodcompTable$correct, 
                        testData$Temp,
                        testData$Wind,
                        testData$goodOzone,
                        ksvm_goodcompTable$Pred)

colnames(plot_ksvm) <- c("correct","Temp","Wind","goodOzone","Predict")
ksvm_ggplot <- ggplot(plot_ksvm, aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=correct,color=goodOzone,shape=Predict))+
  ggtitle("ksvm - good/bad ozone")
ksvm_ggplot

# Build a model based on svm
svmgood <- svm(goodOzone~., data=trainData, kernel="linear", cost=10, scale=FALSE)
svmgood

# Predict data based on data from the model/svmoutput
# & testData
svm_goodPred <- predict(svmgood, testData)
svm_goodPred # This should yield a 0/1

# Create a comparison data-frame that contains the testData for Ozone
# & predicted values using the ksvm() function
svm_goodcompTable <- data.frame(testData[,6], svm_goodPred)
colnames(svm_goodcompTable) <- c('Test', 'Pred')
svm_goodcompTable

# Calculate the percentage of correct values (this is different from the 
# linear/regression models where we calculate RMSE)
percentage_svm <- length(which(svm_goodcompTable$Test == svm_goodcompTable$Pred))/dim(svm_goodcompTable)[1]
percentage_svm
# Percentage = 0.80392

# Confusion matrix
results <- table(Test=svm_goodcompTable$Test, Pred=svm_goodcompTable$Pred)
print(results)

# Plot the results
svm_goodcompTable$correct <- ifelse(svm_goodcompTable$Test==svm_goodcompTable$Pred,"correct","wrong")
plot_svm <- data.frame(svm_goodcompTable$correct, 
                        testData$Temp,
                        testData$Wind,
                        testData$goodOzone,
                        svm_goodcompTable$Pred)

colnames(plot_svm) <- c("correct","Temp","Wind","goodOzone","Predict")
svm_ggplot <- ggplot(plot_svm, aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=correct,color=goodOzone,shape = Predict))+
  ggtitle("svm - good/bad ozone")
svm_ggplot

# Build a model based on Naive Bayes algorithm
nbgood <- naiveBayes(goodOzone~., data=trainData)
nbgood

# Predict data based on data from the model/svmoutput
# & testData
nb_goodPred <- predict(nbgood, testData)
nb_goodPred # This should yield a 0/1

# Create a comparison data-frame that contains the testData for Ozone
# & predicted values using the ksvm() function
nb_goodcompTable <- data.frame(testData[,6], nb_goodPred)
colnames(nb_goodcompTable) <- c('Test', 'Pred')
nb_goodcompTable

# Calculate the percentage of correct values (this is different from the 
# linear/regression models where we calculate RMSE)
percentage_nb <- length(which(nb_goodcompTable$Test == nb_goodcompTable$Pred))/dim(nb_goodcompTable)[1]
percentage_nb
# Percentage = 0.7843

# Confusion matrix
results <- table(Test=nb_goodcompTable$Test, Pred=nb_goodcompTable$Pred)
print(results)

# Plot the results
nb_goodcompTable$correct <- ifelse(nb_goodcompTable$Test==nb_goodcompTable$Pred,"correct","wrong")
plot_nb <- data.frame(nb_goodcompTable$correct, 
                       testData$Temp,
                       testData$Wind,
                       testData$goodOzone,
                       nb_goodcompTable$Pred)

colnames(plot_nb) <- c("correct","Temp","Wind","goodOzone","Predict")
nb_ggplot <- ggplot(plot_nb, aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=correct,color=goodOzone,shape = Predict))+
  ggtitle("nb - good/bad ozone")
nb_ggplot

# Conclusion:
  #  - Percentage of accuracy for svm(80%) is higher than ksvm(68%) & nb(78%)
  # For the given data-set, SVM is a better algorithm than KSVM & Naive Bayes 

# Using gridExtra to represent graphs in one plane
grid.arrange(ksvm_ggplot, svm_ggplot, nb_ggplot, nrow=2)
