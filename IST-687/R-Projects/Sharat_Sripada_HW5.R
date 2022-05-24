#
#      Course: IST-687
#      Name: Sharat Sripada
#      Homework #4
#      Due Date: 2/9/2020
#      Date Submitted: 2/9/2020
#      Topic: JSON & tapply Homework: Accident Analysis


# install.packages("RCurl")
# install.packages("curl")
# install.packages("stringr")
library("RCurl")
library("sqldf")
library("jsonlite")
library("stringr")

# Load the data
url <- "https://opendata.maryland.gov/resource/pdvh-tf2u.json"
document<-fromJSON(txt=url)
str(document)
# > str(document)
# 'data.frame':	1000 obs. of  18 variables:
# .
# .

# Cleansing the data (2x Steps as below)
document_cleanse <- document

# Step-1: Omit all NAs
document_cleanse_omit_nas <- na.omit(document)
str(document_cleanse_omit_nas)
# > str(document_cleanse_omit_nas)
# 'data.frame':	876 obs. of  18 variables:
# .
# .

# Step-2: Remove spaces from a few columns like day_of_week
document_cleanse$day_of_week <- str_replace(document_cleanse$day_of_week, "\ .*","")
document_cleanse_omit_nas$day_of_week <- str_replace(document_cleanse_omit_nas$day_of_week, "\ .*","")

# From Prof Kudy's session - you can use gsub(" ", "", document_cleanse$day_of_week)

# Use the sqldf function of R to interpret the data-frame 
# using SQL commands
# How many accidents happen on SUNDAY 
sqldf("select count(day_of_week) from document_cleanse where day_of_week=='SUNDAY'")

# From Prof Kudy's session (using the TRIM function)
sqldf("select count(day_of_week) from document_cleanse where TRIM(day_of_week)='SUNDAY'")

# How many accidents had injuries
sqldf("select count(injury) from document_cleanse where injury=='YES'")

# Remove NAs from the data & get the counts again
sqldf("select count(day_of_week) from document_cleanse_omit_nas where day_of_week=='SUNDAY'")
sqldf("select count(injury) from document_cleanse_omit_nas where injury=='YES'")

# Using tapply to achieve the same tasks
tapply(document_cleanse$day_of_week, document_cleanse$day_of_week=='SUNDAY', length)
tapply(document_cleanse$injury, document_cleanse$injury=='YES', length)

# List the injuries by day
tapply(document_cleanse$injury, list(document_cleanse$day_of_week, document_cleanse$injury == 'YES'), length)

