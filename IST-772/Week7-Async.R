# Correlation & Variance

x <- c(1,2,3)
y <- c(1,3,2) # corr = .5
x <- c(1,2,3)
y <- c(3,1,2) # corr = -.5
plot(x,y)
cor(x,y)

# correlation matrix
pairs(iris) # Pictorial representation of cross-correlation for all vars
pairs(iris[,1:4]) # Skip the categorical var
cor(iris[,1:4])
round(cor(iris[,1:4]), 2)


# r is a sample statistic/correlation co-eff
# rho/P is the population correlation co-eff
# Null Hypothesis: The population correlation is zero

wood <- rnorm(24)
heat <- rnorm(24)
cor.test(wood,heat)


bfCorTest <- function (x,y) # Get r from BayesFactor
{
  zx <- scale(x) # Standardize X
  zy <- scale(y) # Standardize Y
  zData <- data.frame(x=zx,rhoNot0=zy) # Put in a data frame
  bfOut <- generalTestBF(x ~ rhoNot0, data=zData) # linear coefficient
  mcmcOut <- posterior(bfOut,iterations=10000) # posterior samples
  print(summary(mcmcOut[,"rhoNot0"])) # Get the HDI for rho
  return(bfOut) # Return Bayes factor object
}

bfCorTest(iris[,"Sepal.Length"],iris[,"Sepal.Width"])

#
# Bayesian test for categorical data - Titanic Dataset
badBoatMF <- ftable(Titanic, row.vars = 2, col.vars = 'Survived')
ctBFout <- contingencyTableBF(badBoatMF, sampleType = 'poisson', posterior=F)
ctBFout

# Analysis of proportions
ctMCMCout <- contingencyTableBF(badBoatMF, sampleType = 'poisson', posterior = T,
                                iterations=10000)
summary(ctMCMCout)

# maleProp
maleProp <- ctMCMCout[, "lambda[1,1]"] / ctMCMCout[, "lambda[1,2]"]
# femaleProp
femaleProp <- ctMCMCout[, "lambda[2,1]"] / ctMCMCout[, "lambda[2,2]"] 

diffProp <- maleProp - femaleProp
hist(diffProp)
quantile(diffProp, c(0.025))
quantile(diffProp, c(0.975))
mean(diffProp)

# Class work - Chi-sq Clinic
class(HairEyeColor)

HEcombined <- HairEyeColor[,,1] + HairEyeColor[,,2]
sum(HEcombined)
class(HEcombined)
HEcombined/sum(HEcombined)


# Do the Chi-sq test
chiOut <- chisq.test(HEcombined)
chiOut

