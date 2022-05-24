#
#      Course: IST-687
#      Name: Sharat Sripada
#      Homework #8
#      Due Date: 3/8/2020
#      Date Submitted: 3/7/2020
#      Topic: Making predictions

library("gdata")

# Step-1: Read the xls
antelopes <- read.xls("/Users/ssharat/Downloads/mlr01_2_2_2_2_2_2.xlsx")
summary(antelopes)
View(antelopes)

# Step-2: Rename the columns:
# X1 -> numfawn
# X2 -> popadultant
# X3 -> anprecip
# X4 -> wintergrade
colnames(antelopes) <- c('numfawn', 'popadultant', 'anprecip', 'wintergrade')
colnames(antelopes)

# Step-3: str()
str(antelopes)

# Step-4: Create bivariate plots
# baby fawns vs adult antelope population
# pch: point character - 15: square, 16: circle etc
# col: color
plot(antelopes$popadultant, antelopes$numfawn, pch=16, col='red')

# baby fawns versus precipitation
plot(antelopes$anprecip, antelopes$numfawn, pch=16, col='red')

# baby fawns versus severity of winter
plot(antelopes$wintergrade, antelopes$numfawn, pch=16, col='red')

# Step-5: Create 3 regression models
# Model-1: predict the number of fawns from the severity of the winter
model1 <- lm(formula=numfawn ~ wintergrade, data=antelopes)
summary(model1)
summary(model1)$r.squared
# Summary for model1:
# Co-efficients:
# wintergrade P-val: 0.0362 < 0.05
# R-square: 0.5459 (which shows not very strong correlation)
# numfawn(Y) = -0.3379 * wintergrade(X) + 3.4966
test <- data.frame(wintergrade=2)
predict(model1, test, type="response")
# Prediction: 2.82 (actual-data: 2.9, 2.3)

# Model-2: predict the number of fawns from the severity of the winter and precipitation
model2 <- lm(formula=numfawn ~ wintergrade + anprecip, data=antelopes)
summary(model2)
summary(model2)$r.squared
# Summary for model2:
# Co-efficients:
# wintergrade P-val: 0.18843 > 0.1
# anprecip P-val: 0.00843 < 0.01
# R-square: 0.9 (which shows strong correlation)
test <- data.frame(wintergrade=2, anprecip=13.2)
predict(model2, test, type="response")
# Prediction: 3.06 (actual-data: 2.9)

# Model-3: predict the number of fawns from the severity of the winter, precipitation, adult population
model3 <- lm(formula=numfawn ~ wintergrade + anprecip + popadultant, data=antelopes)
summary(model3)
summary(model3)$r.squared
# Summary for model3:
# Co-efficients:
# wintergrade P-val: 0.0466 < 0.05
# anprecip P-val: 0.0217 < 0.05
# popadultant P-val: 0.0273 < 0.05
# R-square: 0.973 (which shows very strong correlation)
test <- data.frame(wintergrade=2, anprecip=13.2, popadultant=9.2)
predict(model1, test, type="response")
# Prediction: 2.82 (actual-data: 2.9)

# So, the best model here model-3 - theoretical & prediction is very close. 

# Step-5: Parsimonious model using the step() function
model <- lm(formula=numfawn ~ ., data=antelopes)
step(model, data=antelopes, direction="backward")
# The step() function showed a single iteration 
# with all three variables - wintergrade, popadultant, anprecip.
