# Week-6 HW
InsectSprays

summary(InsectSprays)
dim(InsectSprays)
str(InsectSprays)

insectOut <- aov(InsectSprays$count ~ InsectSprays$spray)
summary(insectOut)

insect_df <- data.frame(InsectSprays$count, InsectSprays$spray)
insectBayesOut <- anovaBF(InsectSprays.count ~ InsectSprays.spray, 
                          data=insect_df)
insectBayesOut
mcmcOut <- posterior(insectBayesOut,iterations=10000)
summary(mcmcOut)


# Doing a t-test with groups-c and f
grpc <- insect_df[insect_df$InsectSprays.spray == 'C', 1]
grpf <- insect_df[insect_df$InsectSprays.spray == 'F', 1]

t.test(grpc, grpf)
