# 
install.packages("BayesFactor")
library("BayesFactor")
set.seed(10)  
precipAmount <- sample(precip,60,replace=TRUE)
precipGrp <- as.factor(rep(seq(from=1,to=3,by=1),20))
precipDF <- data.frame(precipAmount, precipGrp)
precipBayesOut <- anovaBF(precipAmount ~ precipGrp, data=precipDF)
precipBayesOut
mcmcOut <- posterior(precipBayesOut,iterations=10000)
summary(mcmcOut)

precipDF$precipAmount[precipDF$precipGrp==3] <- precipDF$precipAmount[precipDF$precipGrp==3] - 7
precipBayesOut <- anovaBF(precipAmount ~ precipGrp, data=precipDF)
precipBayesOut
mcmcOut <- posterior(precipBayesOut,iterations=10000)
summary(mcmcOut)
