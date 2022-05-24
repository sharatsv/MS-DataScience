# Sampling
jar <- c(0,1)
numSamples <- 4
# Replace = TRUE
sample(jar, numSamples, replace=TRUE)
# Replace = FALSE
sample(jar, numSamples) # Generates an error since the numSamples > num elements in jar
sample(jar, 2)

# Using replicate to repeat the sample process n times
# simplify = TRUE -> return the data as a data-frame
replicate(5, sample(jar, numSamples, replace=TRUE), simplify = TRUE)

# To test the Central Limit Theorem calculate mean
sampleMean <- replicate(5, mean(sample(jar, numSamples, replace=TRUE)), simplify = TRUE)
mean(sampleMean)

# Population mean is 0.5
mean(jar)

# To make the sample mean tend towards population mean do:
# - numSamples = 50
# - replicate tests many times
numSamples <- 50
sampleMean <- replicate(10, mean(sample(jar, numSamples, replace=TRUE)), simplify = TRUE)
mean(sampleMean)

# Plot the means using a histogram
hist(sampleMean)

# Exercise: Create a jar var with (-1,0,1) & sample it 250x. What's the mean?
jar <- c(-1,0,1)
numSamples <- 250
sampleMeans <- mean(sample(jar, numSamples, replace = TRUE))
# > sampleMeans
# [1] 0.052

install.packages("moments")
skewness(sampleMeans)
