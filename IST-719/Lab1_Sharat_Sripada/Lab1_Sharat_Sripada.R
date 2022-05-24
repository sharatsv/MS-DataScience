# Lab-1 - Week-1

# Plot a vector c(7,4,2,12)
pie(c(7,4,2,12))

# Assign the vector to a var
x <- c(7,4,2,12)
pie(x)

# Add title
pie(x, main = "Sharat's Pie")

# Add colors
pie(x, main = "Sharat's Pie", col=c("red", "orange", "tan", "yellow"))

# Add labels
pie(x
    , main = "Sharat's Pie", col=c("red", "orange", "tan", "yellow")
    , labels = c("a", "b", "c", "d"))


# Make a dot plot
plot(c(1,3,6,4))

# Add points
plot(c(1,3,6,4), pch=16, col=c("red", "orange", "tan", "yellow")
     , cex=3)

# Random normal distribution of data
x <- rnorm(n = 10)

# Create a plot 
plot(x)

# Create a plot of type line (l)
plot(x, type='l')

# Create a plot of type histogram (h)
plot(x, type='h')

# Size and color the histogram
plot(x, type='h', lwd =  5, lend = 2, col = "orange")

# Add title and labels
plot(x, type='h', lwd =  5, lend = 2, col = "orange"
     , main = "change in net worth"
     , xlab = "time in years"
     , ylab = "in millions"
     , bty = "n"
     )

# Use par() to control params of the plot
par(bg = "gray")
plot(x, type='h', lwd =  20, lend = 2, col = c("blue", "orange")
     , bty = "n"
     )

# Grab using sample(), 27 chars from first 3 letters
n <- 27
my.letters <- sample(letters[1:3], size = n, replace = T)

# Count using table()
tab <- table(my.letters)

# Plot a bar graph
barplot(tab, col = c("brown", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "black"
        , xlab = "departments"
        , ylab = "employees"
        , main = "Company Employees"
        , horiz = TRUE
        , las = 1
)

# Instead of solid fill, fill with lines 
barplot(tab, col = c("brown", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "black"
        , xlab = "departments"
        , ylab = "employees"
        , main = "Company Employees"
        , horiz = TRUE
        , las = 1
        , density = 20
        , angle = c(45, 90, 12)
)


# Create some data using rnorm()
x <- rnorm(n = 1000, mean = 10, sd = 1)

# Plot a histogram
hist(x, main = "what is the distribution of x")

# Box-plot
boxplot(x, horizontal = T)

x <- rlnorm(n = 1000, meanlog = 1, sdlog = 1)

# mfrow() helps create a plot canvas of 2-rows x 1-col 
par(mfrow = c(2,1))
boxplot(x, horizontal = T)
hist(x)















