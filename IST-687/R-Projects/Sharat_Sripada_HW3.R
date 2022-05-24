#
#      Course: IST-687
#      Name: Sharat Sripada
#      Homework #3 (Cleaning/Munging Dataframes)
#      Due Date: 2/2/2020
#      Date Submitted: 2/2/2020
#

# Step-1: Create a function (named readStates) to read a CSV file into R
# URL as below:
# http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv

readStates <- function(get_url)
{
  x <- read.csv(url(get_url))
  # Step-2: Clean the dataframe
  # Remove not-relevant rows/columns
  # Step-2.1: Remove first 8-rows 
  x <- x[-1:-8,]
  # Step-2.2: Looking at summary(x) last 5-columns are NAs - Remove them.
  x <- x[,-6:-10]
  # Step-2.3: Last 7-rows seem like not useful data - Remove them.
  # dims(x) -> 58 5 
  x <- x[-52:-58,]
  # Step-2.4: Rename rows & columns
  cnames <- colnames(x)
  cnames[1] <- "stateName"
  cnames[2] <- "base2010"
  cnames[3] <- "base2011"
  cnames[4] <- "Jul2010"
  cnames[5] <- "Jul2011"
  colnames(x) <- cnames
  rownames(x) <- NULL
  # Step-2.5: Normalize the data (to strings/chars & numeric)
  x$stateName <- gsub("\\.","",x$stateName)
  x$base2010 <- as.numeric(gsub(",","",x$base2010))
  x$base2011 <- as.numeric(gsub(",","",x$base2011))
  x$Jul2010 <- as.numeric(gsub(",","",x$Jul2010))
  x$Jul2011 <- as.numeric(gsub(",","",x$Jul2011))
  # NOTE: str(x) as below:
  # 'data.frame':	51 obs. of  5 variables:
  #$ stateName: chr  "Alabama" "Alaska" "Arizona" "Arkansas" ...
  #$ base2010 : num  4779736 710231 6392017 2915918 37253956 ...
  #$ base2011 : num  4779735 710231 6392013 2915921 37253956 ...
  #$ Jul2010  : num  4785401 714146 6413158 2921588 37338198 ...
  #$ Jul2011  : num  4802740 722718 6482505 2937979 37691912 ...
  return(x)
}

# Step-1: Call a function that reads csv data from URL and cleans up data
my_url <- 'http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv'
my_raw_data <- readStates(my_url)


# Step-3: Clean the dataframe
dfStates <- data.frame(my_raw_data)
mean(dfStates$Jul2011)

# Step-4: Find the state with the Highest Population
max(dfStates$Jul2011)
max_index <- which.max(dfStates$Jul2011)
# > dfStates[max_index,]
# stateName base2010 base2011  Jul2010  Jul2011
# 5 California 37253956 37253956 37338198 37691912
dfStates[max_index, 1]
dfStates[order(dfStates$Jul2011),]

# Step-5: Explore the distribution of the states
# All vectors and vars assosicated with func percentlowerthan
# will be plt_<var>
percentlowerthan <- function(plt_val, plt_vector)
{
  plt_num_below_val <- length(plt_vector[plt_vector < plt_val])
  plt_percent <- plt_num_below_val / length(plt_vector) * 100
  return(plt_percent)
}

dfStatesJul2011Mean <- mean(dfStates$Jul2011)
dfStatesJul2011Vector <- dfStates$Jul2011
percentlowerthan(dfStatesJul2011Mean, dfStatesJul2011Vector)

