#
#      Course: IST-687
#      Name: Sharat Sripada
#      Homework #4
#      Due Date: 2/9/2020
#      Date Submitted: 2/9/2020
#      Topic: Samples HW

# Install moments package for skewness calculation
# install.packages("moments")

# Step1-1: Summarizing function to understand distribution of a vector
# Step1-2: Calculate mean, min, max, sd, quantile & skewness
printVecInfo <- function(input){
  my_mean <- mean(input)
  my_median <- median(input)
  my_min <- min(input)
  my_max <- max(input)
  my_sd <- sd(input)
  my_quantile <- quantile(input, probs=c(0.05, 0.95))
  library("moments")
  my_skewness <- skewness(input)
  cat("Mean:", my_mean, "Median:", my_median, "Min:", my_min, 
      "Max:", my_max, "Std.Dev:", my_sd,
      "Quantile (0.05-0.95):", my_quantile, 
      "Skewness:", my_skewness)
}

# Step1-3:
# Create a vector with c(1,2,3,4,5,6,7,8,9,10,50) & call 
# function printVecInfo
myData <- c(1,2,3,4,5,6,7,8,9,10,50)
printVecInfo(myData)


# Step2-4:
# Create a jar var with 50x red & 50x blue marbles/strings
red <- replicate(50, 'red')
blue <- replicate(50, 'blue')
jar <- c(red, blue)

# Step2-5:
# Count if there are 50 red marbles in the jar
count_red <- length(jar[jar == 'red'])
if (count_red == 50) "There are 50 red marbles in the jar!"

# Step2-6:
# Sample 10 marbles from the jar & count % of red marbles
sampleSize <- 10
sampleSet <- sample(jar, sampleSize, replace = TRUE)
sampleSet
count_red_sample <- length(sampleSet[sampleSet == 'red'])
count_red_sample
percent_red <- count_red_sample/sampleSize * 100
percent_red

# Step2-7:
# Sample the jar 20x times using the replicate() function
# with sampleSize <- 10, each time counting red marbles in the sample. 
# Method to count red marbles in jar:
# - grep for "red" - grep("red", sample(jar, sameplSize, replace=TRUE))
# - count using length
meanSamples <- replicate(20, mean(length(grep("red", sample(jar, sampleSize, replace = TRUE))), 
                                  simplify = TRUE))
printVecInfo(meanSamples)
hist(meanSamples)

# Step2-8:
# Repeat with replicate with a larger sampleSize (sample = 100)
sampleSize <- 100
meanSamples <- replicate(20, mean(length(grep("red", sample(jar, sampleSize, replace = TRUE))), 
                                  simplify = TRUE))
printVecInfo(meanSamples)
hist(meanSamples)

# Step2-9: 
# Repeat with larger replication size (replicate 100x times)
meanSamples <- replicate(100, mean(length(grep("red", sample(jar, sampleSize, replace = TRUE))), 
                                  simplify = TRUE))
printVecInfo(meanSamples)
hist(meanSamples)

# Step3-10:
# Store airquality data-set into a temp. var
data()
aq <- airquality

# Step3-11:
# Clean the data-set (remove NAs)
summary(aq)
dim(aq)
# Omit NAs from the data-set
# NOTE - This will remove the row in full!
aq_omit_na <- na.omit(aq)
dim(aq_omit_na)
# Step3-12:
# Explore Ozone data by calling printVecInfo & hist()
aq_ozone <- aq_omit_na$Ozone
printVecInfo(aq_ozone)
hist(aq_ozone)
# Explore Wind data by calling printVecInfo & hist()
aq_wind <- aq_omit_na$Wind
printVecInfo(aq_wind)
hist(aq_wind)
# Explore Temp data by calling printVecInfo & hist()
aq_temp <- aq_omit_na$Temp
printVecInfo(aq_temp)
hist(aq_temp)









