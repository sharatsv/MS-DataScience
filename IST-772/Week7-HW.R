# Week-7 HW
# Pages: 155-156, Problems: 3-4, 8-10

# Load the rock data-set
rock
cor.test(rock$area, rock$perm)


# Bayesian test on correlation coeff.
library('BayesFactor')

bfCorTest <- function(x, y) {
  zx <- scale(x) # scale() normalizes or standardizes the data - Essentially: (xi - x-bar) / SD
  zy <- scale(y)
  zData <- data.frame(x=zx, rhoNot0=zy)
  bfOut <- generalTestBF(x ~ rhoNot0, data=zData)
  # Run the MCMC simulation
  mcmcOut <- posterior(bfOut, iterations=10000)
  print(summary(mcmcOut[,'rhoNot0']))
  return(bfOut)
}

bfCorTest(rock$area, rock$perm)


# Chi-sq test on UCBAdmissions data set
UCBAdmissions
chisq.test(UCBAdmissions[,,1], correct=F)


# Bayesian approach to Chi-sq test
ctBFout <- contingencyTableBF(UCBAdmissions[,,1], sampleType='poisson', posterior = F)
ctBFout


# Bayesian approach to Chi-sq test
#   - run with posterior sampling
#.  - calculate 95% HDI of the diff in proportions between the cols
ctMCMCout <- contingencyTableBF(UCBAdmissions[,,1], sampleType='poisson', posterior = T,
                                iterations=10000)
summary(ctMCMCout)
# Male proportions
maleProp <- ctMCMCout[,"lambda[1,1]"]/ctMCMCout[,"lambda[2,1]"]

# Female proportions
femaleProp <- ctMCMCout[,"lambda[1,2]"]/ctMCMCout[,"lambda[2,2]"]

# Diff proportions
diffProp <- maleProp - femaleProp

hist(diffProp)
abline(v=quantile(diffProp, c(0.025), color='black'))
abline(v=quantile(diffProp, c(0.975), color='black'))
mean(diffProp)
 
