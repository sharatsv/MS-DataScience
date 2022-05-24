# List all data-sets in RStudio
data()
# To describe data-set
?mtcars

# Alternative to structure/str(mtcars)
dim(mtcars)

myCars <- mtcars

# Mean calculation
mean(myCars$mpg)

# Different ways to get columns
myCars[9]
myCars[[9]]
myCars["am"]
myCars$am

# Splicing data
myownmtcars<-mtcars[,c("hp","am")]

meanMpg <- mean(myCars$mpg)

