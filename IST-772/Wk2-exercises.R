# Week-2: Probability

rbinom(n=20, size=6, prob = 0.5)
hist(rbinom(n=20, size=6, prob = 0.5))

rbinom(n=100, size=6, prob = 0.5)
table(rbinom(n=100, size=6, prob = 0.5))

# To generate proportions or probability of each
table(rbinom(n=100, size=6, prob = 0.5))/100

# Visualizing tables of random data with binomial probability
probtable <- table(rbinom(n=100, size=6, prob = 0.5))/100
barplot(probtable)

# Cumulative sum/Accumulated probability
cumsum(probtable)
# > cumsum(probtable)
# 0    1    2    3    4    5    6 
# 0.02 0.15 0.40 0.67 0.88 0.99 1.00
# Explanation: Read this as - Probability of say, 3 or fewer face-down events is 0.67

barplot(cumsum(probtable))


# Contingency table
# A table of minimum size 2 x 2 that shows combinations of event outcomes.


# Week-2: Sync exercise
pascals <- lapply(0:7, function(i) choose(i, 0:i))
pascals_prob <- lapply(0:7, function(i) choose(i, 0:i)/sum(choose(i, 0:i)))

rbinom(n=10, size=7, prob = 0.5)
#  [1] 3 2 2 5 3 5 3 3 2 3
# The probability of 0 or 7 (that is of 0 or 7 heads is 1/128 )

table(rbinom(n=100000, size=7, prob = 0.5))
#     0     1     2     3     4     5     6     7 
#    788  5474 16371 27431 27459 16301  5428   748 

barplot(table(rbinom(n=100000, size=7, prob = 0.5))/100000)
#       0       1       2       3       4       5       6       7 
#    0.00752 0.05602 0.16414 0.27521 0.27245 0.16205 0.05456 0.00805 

dbinom(x=c(0,1,2,3,4,5,6,7),size=7,prob=0.5)

