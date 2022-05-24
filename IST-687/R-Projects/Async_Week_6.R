# Async - Week6

library(ggplot2)

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

my_url <- 'http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv'
dfStates <- readStates(my_url)
ggplot(dfStates, aes(x=Jul2011)) + geom_histogram(bins=30)
ggplot(dfStates, aes(x=Jul2011)) + geom_histogram(binwidth = 1000000, color="black", fill="white")
ggplot(dfStates, aes(x=factor(0), Jul2011)) + geom_boxplot()

# Playing with bins and binwidth + geom_histogram()
# bins = <> is the number of columns of bins
# binwidth = <> is the width of each bin
mtc <- mtcars
g <- ggplot(mtc, aes(x=mpg)) + geom_histogram(bins=10)
g

g <- ggplot(mtc, aes(x=mpg)) + geom_histogram(bins=10, color="black", fill="white")
g

g <- ggplot(mtc, aes(x=mpg)) + geom_histogram(binwidth=10, color="black", fill="white")
g

# Line plots/geom_line()
timeToNYC <- c(4,4.5,3.5,5,4,4.2)
timeToNYCWeek2 <- c(4.5,5,3.8,5.2,4.6,4.3)
day <- c("mon","tues","wed","thurs","fri","sat")
week1 <- c(1,1,1,1,1,1)
week2 <- c(2,2,2,2,2,2)
time <- c(timeToNYC, timeToNYCWeek2)
week <- c(week1, week2)
dayofWeek <- c(day, day)
df <- data.frame(day, timeToNYC, timeToNYCWeek2)
df
g <- ggplot(df, aes(x=day, y=timeToNYC, group=1)) + geom_line()
g

# Add each data-point as a point on the dashed-line
g <- ggplot(df, aes(x=day, y=timeToNYC, group=1)) + geom_line(color="red", linetype="dashed", size=1)
g <- g + geom_point()
g

# Enhance each data-point as a bigger point on the dashed-line
g <- ggplot(df, aes(x=day, y=timeToNYC, group=1)) + geom_line(color="red", linetype="dashed", size=1)
g <- g + geom_point(color="black", size=4)
g

# Add a y-label on the graph
g <- ggplot(df, aes(x=day, y=timeToNYC, group=1)) + geom_line(color="red", linetype="dashed", size=1)
g <- g + geom_point(color="black", size=4) + ylab("time to NYC(in hours)")
g

df <- data.frame(dayofWeek, time, week)
df
# Plot multiple line plots (group by week) 
g <- ggplot(df, aes(x=dayofWeek, group=week, color=week)) + geom_line(aes(y=time))
g


# Bar and scatter plots
# Create a box-plot of mpg
# How to read a box-plot - From the plot beside:
#  - min: 10.625
#  - max: ~33.75
#  - median: 18.75 (middle horizontal line in the box)
#  - 1st quartile: 15.125 (lower horizontal line in the box)
#  - 3rd quartile: 22.625 (upper horizontal line in the box)
ggplot(mtc, aes(x=factor(0), mpg)) + geom_boxplot()

# Add x-axis as cyl and y-axis as mpg & group by cyl (similar to the tapply function)
ggplot(mtc, aes(group=cyl, x=cyl, mpg)) + geom_boxplot()

# Flip co-ordinates
ggplot(mtc, aes(group=cyl, x=cyl, mpg)) + geom_boxplot() + coord_flip()

# Create a bar-chart
car.names <- rownames(mtc)
# If you don't use stat = "identity" then you will error:
# Error: stat_count() must not be used with a y aesthetic.
g <- ggplot(mtc, aes(x=car.names, y = wt)) + geom_bar(stat = "identity") 
g

# X-axis labels are all messed up. Make it readable.
g <- g + theme(axis.text.x = element_text(angle=90, hjust=1))
g

# Group by cyl & plot #of gears per cyl usin the fill utility
ggplot(mtc, aes(x=cyl, fill=factor(gear))) + geom_bar()

# Scatter-plots
ggplot(mtc, aes(x=mpg, y=car.names)) + geom_point(size=3)

       