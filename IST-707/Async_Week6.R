# Week-6 Async

library(rpart)
library(rattle)

titanic <- read.csv('/Users/venkatasharatsripada/Documents/Masters@Syracuse/Course-Related(Study)/IST-707/week2_resources_2_2_2_2_2/Titanic_Training_Data.csv')
# test_titanic <- read.csv('/Users/venkatasharatsripada/Documents/Masters@Syracuse/Course-Related(Study)/IST-707/week2_resources_2_2_2_2_2/Titanic_Testing_Data.csv')

train_rows <- as.integer(0.8 * dim(titanic)[1])
train_titanic <- titanic[1:train_rows,]
dim(train_titanic)
test_titanic <- titanic[(train_rows+1):dim(titanic)[1],]
dim(test_titanic)

# Using r-part to build a decision tree for the train-data
my_dt1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train_titanic, method='class')
summary(my_dt1)

# Visualizations 
fancyRpartPlot(my_dt1)
printcp(my_dt1)
#         CP nsplit rel error  xerror     xstd
# 1 0.444444      0   1.00000 1.00000 0.042446
# 2 0.030702      1   0.55556 0.55556 0.035750
# 3 0.023392      3   0.49415 0.50000 0.034372
# 4 0.020468      4   0.47076 0.49708 0.034295
# 5 0.010234      5   0.45029 0.50000 0.034372
# 6 0.010000      8   0.41813 0.50000 0.034372
# Choose the least cross-validated error (xerror) which is:
#  - 4 0.020468      4   0.47076 0.49708 0.034295

# This gives the minimum CP associated with minimum cross-validated error
my_dt1$cptable[which.min(my_dt1$cptable[,"xerror"]),"CP"]

plotcp(my_dt1)

# Prediction based on unpruned tree
my_pred <- predict(my_dt1, test_titanic, type='class')
comp_table <- data.frame(PassengerId=test_titanic$PassengerId, Survived=test_titanic$Survived, 
                         Pred_Survived=my_pred)
# Table it to visualize
table(Survived=comp_table$Pred_Survived, true=comp_table$Survived)
#         true
#  Survived   0   1
#  0 111  17
#  1   4  47

# Check with confusion matrix as well
# library(caret)
confusionMatrix(as.factor(comp_table$Pred_Survived), as.factor(comp_table$Survived))

# Accuracy : 0.8827 

# Prune the tree based on min CP/xerror
# Size of tree = 4
ptree <- prune(my_dt1, cp=my_dt1$cptable[which.min(my_dt1$cptable[,"xerror"]),"CP"])

fancyRpartPlot(ptree)

printcp(ptree)

# Prediction based on pruned tree
my_pred_prune <- predict(ptree, test_titanic, type='class')
comp_table_prune <- data.frame(PassengerId=test_titanic$PassengerId, Survived=test_titanic$Survived, 
                               Pred_Survived=my_pred_prune)

confusionMatrix(as.factor(comp_table_prune$Pred_Survived), as.factor(comp_table_prune$Survived))

# Accuracy : 0.8659 


# Use Naive-Bayes to predict the outcome of Survivors
install.packages('e1071')
library(e1071)

# Convert the Survived to factor and Pclass to ordered
train_titanic$Survived <- factor(train_titanic$Survived)
train_titanic$Pclass <- ordered(train_titanic$Pclass)

test_titanic$Survived <- factor(test_titanic$Survived)
test_titanic$Pclass <- ordered(test_titanic$Pclass)

nb <- naiveBayes(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train_titanic, laplace = 1, na.action=na.pass)
my_pred_nb <- predict(nb, newdata=test_titanic, type=c('class'))

comp_table_nb <- data.frame(PassengerId=test_titanic$PassengerId, Survived=test_titanic$Survived, 
                         Pred_Survived=my_pred_nb)

library(caret)
confusionMatrix(as.factor(comp_table_nb$Pred_Survived), as.factor(comp_table_nb$Survived))
