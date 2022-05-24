# LiveClass-5 breakout
fname <- file.choose()
sales <- read.csv(file=fname, header=TRUE, stringsAsFactors = F)

colnames(sales)
attach(sales)
tapply(income, year, sum)
tapply(income, list(rep.region, year), sum)
tapply(units.sold, list(type,rep.region), sum)

# Async Week-5 examples with SQL
install.packages("sqldf")
library("sqldf")
results <- sqldf('select mtcars.mpg from mtcars')
str(results)
mean(mtcars$mpg)
# SQL examples:
# SQL command to get mpg for cars where cyl=4
sqldf('select mtcars.mpg from mtcars where cyl=4')

# Using SQL commands to get the mean of mpg where cyl=4
sqldf('select AVG(mtcars.mpg) from mtcars where cyl=4')

# OR
sqldf('select AVG(mtcars.mpg) from mtcars group by cyl')

# Alternatively, substitute using the tapply R function
tapply(mtcars$mpg, mtcars$cyl, mean)

# Example of getting data for a cyl type alone
tapply(mtcars$mpg, mtcars$cyl==4, mean) 
# Output as below (groups )
#    FALSE     TRUE 
#  16.64762 26.66364

results <- tapply(mtcars$mpg, mtcars$cyl==4, mean) 
str(results)
#  num [1:2(1d)] 16.6 26.7
# - attr(*, "dimnames")=List of 1
# ..$ : chr [1:2] "FALSE" "TRUE"
# NOTE - tapply returns a list. Access it as below:
results[1]
results[2]


