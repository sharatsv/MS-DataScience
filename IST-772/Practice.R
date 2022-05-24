# IST-772

# install.packages('modeest')
library(modeest)
discoveries
mfv(discoveries)


# Inferential stats manually using R
votes <- c(200, 300, 400)

# Sq. deviation
(votes - mean(votes)) ^ 2

# Sum of sq.
sum((votes - mean(votes)) ^ 2)

# Variance = Sum of sq/# of obs
sum((votes - mean(votes)) ^ 2) / length(votes)

# Calculate the SD using the manual method - sqrt(var.)
votes1 <- c(200, 300, 400)
votes2 <- c(299, 300, 301)

sqrt(sum((votes1 - mean(votes1)) ^ 2) / length(votes1))
sqrt(sum((votes2 - mean(votes2)) ^ 2) / length(votes2))

# Using the built-in functions in R to measure dispersion in Data
sd(discoveries)
var(discoveries)
mean(discoveries)
range(discoveries)


# Using the histogram to visualize data
hist(rnorm(n=120, mean=85, sd=5))
hist(rnorm(n=120, mean=85, sd=15))
hist(rnorm(n=120, mean=0, sd=15))

hist(rnorm(n=120, mean=60, sd=5))

# Uniform plots
hist(runif(120, min=50, max=80))
hist(runif(1200, min=50, max=80))

# Poisson distribution - If you stand on a road and observe the arrival times
# of cars, then they will follow a Poisson distribution trend
hist(rpois(120, lambda = 3))
hist(rpois(120, lambda = 30))


# Make your own tables in R
toast <- matrix(c(2,1,3,4), ncol=2, byrow=T)
colnames(toast) <- c('Down', 'Up')
rownames(toast) <- c('Jelly', 'Butter')
toast <- as.table(toast)
margin.table(toast) # Grand total of toast drops
margin.table(toast, 1) # Marginal total for rows
margin.table(toast, 2) # Marginal total for cols

# Calculate probabilities
toastprobs <- toast/margin.table(toast)

# Simulate flipping a coin 100,000 times. Explain the results
coin_flip <- rbinom(n=100000, size=9, prob=0.5)
hist(coin_flip) # See gaps in the histogram since it is trying to plot intermediate values like 1.5, 2.5 etc.

coin_flip_table <- table(coin_flip)
barplot(coin_flip_table)

# Represent the table in probabilities
coin_flip_prob_table <- coin_flip_table/100000
barplot(coin_flip_prob_table)

# Week-3 async: Sampling from population
gumballs <- rep.int(1:2, 25)
gumballs <- factor(gumballs, labels = c('Red', 'Blue'))
gumballs

# Sample 
sample(gumballs, size=10, replace = T)
sum(sample(gumballs, size=10, replace=T) == 'Red')
sum(sample(gumballs, size=1000, replace=T) == 'Red')

# Other available distributions
runif(n=10, min=1, max=5) # random deviates of a uniform function
rnorm(n=10, mean=5, sd=2)  # random deviates of a normal function
rbinom(n=10, size = 1, prob = 0.5) # randome deviates of binominal function


# Toast Angle data: A simulated population
set.seed(5)
toastAngleData <- runif(1000,0,180)
head(toastAngleData)
tail(toastAngleData)
mean(toastAngleData)

hist(toastAngleData)

sample(toastAngleData, size=14, replace = T)
mean(sample(toastAngleData, size=14, replace = T)) # Mean of sample of size 14 - can be considered a trial

# Use replicate to repeat process
samplingDistribution <- replicate(10000, mean(sample(toastAngleData, size=14, replace = T)),
                                  simplify = T)
hist(samplingDistribution)

# Using the quantile function
quantile(0:100,probs=0.75)

# TO-DO: Read more about qnorm
qnorm(0.5)
qnorm(0.75)
qnorm(0.25)
qnorm(0.975)
qnorm(0.025)
