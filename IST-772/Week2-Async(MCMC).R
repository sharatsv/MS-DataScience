# Topics:
#  - Markov Chain Monte Carlo method
# 
# BEST - Runs a simulation called MCMC in order to characterize
# the distances between the means of the two groups. 
# A Markov chain is a statistical technique for examining the
# probabilities of linked events. Monte Carlo refers to a process
# of running repeated simulations  in order to look around for 
# everything in between the 'best case' and 'worst case' scenarios.

data("mtcars")
mt <- mtcars

# Run a statistical t-test between the mpg of AT/MT cars
t.test(mt$mpg[mt$am == 0], mt$mpg[mt$am == 1])

# Run a Bayesian BEST simulation that uses MCMC model
install.packages('BEST')
library(BEST)
carsBest <- BESTmcmc(mt$mpg[mt$am == 0], mt$mpg[mt$am == 1])
plot(carsBest)


pg <- PlantGrowth
ctrl <- pg$weight[pg$group == 'ctrl']
trt1 <- pg$weight[pg$group == 'trt1']
trt2 <- pg$weight[pg$group == 'trt2']
  
plantBest <- BESTmcmc(ctrl, trt1)
plot(plantBest)

# Run t-test and MCMC for ctrlm trt2
t.test(ctrl, trt2)

plantBest2 <- BESTmcmc(ctrl, trt2)
plot(plantBest2)


# Null Hypothesis Significance Test (NHST)
t.test(rnorm(100000, mean=17.1, sd=3.8), rnorm(100000, mean=17.2, sd=3.8))
