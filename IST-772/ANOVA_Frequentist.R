# ANOVA
set.seed(1)
pgrp1 <- sample(precip,20, replace=TRUE)
pgrp2 <- sample(precip,20, replace=TRUE)
pgrp3 <- sample(precip,20, replace=TRUE)
v1 <- var(precip)
v2 <-var(c(pgrp1,pgrp2,pgrp3))
pgrp3 <- pgrp3-5
v3 <- var(c(pgrp1,pgrp2,pgrp3))
barplot(c(v1,v2,v3),names.arg=c("Original Data","3 Samples","3rd Grp+5"))

# ANOVA for precipitation data-set
set.seed(10)
precipAmount <- sample(precip, 60, replace = T)
precipGrp <- as.factor(rep(seq(from=1, to=3, by=1), 20))
precipDF <- data.frame(precipAmount, precipGrp)
boxplot(precipAmount ~ precipGrp, data=precipDF)

precipOut <- aov(precipAmount ~ precipGrp, data=precipDF)
summary(precipOut)

# What does the next line do?
precipDF$precipAmount[precipDF$precipGrp==3] <- precipDF$precipAmount[precipDF$precipGrp==3] - 7
precipOut <- aov(precipAmount ~ precipGrp, data=precipDF)
summary(precipOut)
