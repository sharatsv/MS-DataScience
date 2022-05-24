# Live class-5
# tapply

data()
mycars <- mtcars
# Use attach so that you don't need to write:
# mycars$cyl instead use cyl
# mycars$mpg instead use mpg
attach(mycars) 
tapply(cyl, cyl==8, length)

install.packages("sqldf")
library("sqldf")
mtcars
myCars <- mtcars
myCars$carname <- row.names(myCars)
myCars
str(myCars)
myCars$carname <- format(myCars$carname, justify = "left")
myCars

count6 <- sqldf("select count(*) from myCars where cyl = 6")

# Pay attention to trim() to manage whitespaces


