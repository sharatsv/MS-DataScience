# Week-3 Async - Writing functions

MyMode <- function(myVector)
{
  uniqueValues <- unique(myVector) 
  uniqueCounts <- tabulate(match(myVector, uniqueValues))
  return(uniqueValues[which.max(uniqueCounts)])
}

tinyData <- c(1,2,1,2,3,3,3,4,5,4,5)
MyMode(tinyData)

tinyData <- c(tinyData,5,5,5,1,1,1)
MyMode(tinyData)

tinyData <- c(tinyData,9,9,9,9,9,9,9)
MyMode(tinyData)

# Descriptive stats
# Create a normalized distribution using rnorm
# rnorm(range, mean, std-dev)
a <- rnorm(100, 50, 2)  

# Create a histogram of the data-set
hist(a)
min(a)
max(a)
# Std-deviation
sd(a) 

# Get the right half of the normal distribution
b <- a[a > 50]
hist(b)



