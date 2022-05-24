# HW-3 - Pages 50-51 Problems 2-7

# Problem-2
cw <- ChickWeight
summary(cw)
dim(cw)
head(cw)
mean(cw$weight)
myChkWts <- cw$weight
quantile(myChkWts, 0.5)

# Working on myChkwts
hist(myChkWts)
# Display quantile of 2.5% or 0.025 and 97.5% or 0.975
quantile(myChkWts, c(0.025, 0.975))
mean(myChkWts)
median(myChkWts)


# Sample the weights
cwweights_mean_sample <- replicate(1000, mean(sample(myChkWts, size=11, replace = T)))
hist(cwweights_mean_sample, xlim=c(50, 200))

# Using abline indicate the 2.5% and 97.5% values/cut-points on the histogram
abline(v=quantile(cwweights_mean_sample, c(0.025, 0.975)))


# Repeat with larger sample size
cwweights_mean_sample <- replicate(1000, mean(sample(myChkWts, size=100, replace = T)))
hist(cwweights_mean_sample, xlim=c(90, 150))

# Using abline indicate the 2.5% and 97.5% values/cut-points on the histogram
abline(v=quantile(cwweights_mean_sample, c(0.025, 0.975)))

