# Lab-3 - Week 3
# Setting colors in R

library(RColorBrewer)

fname <- file.choose()

sales <- read.csv(fname, header=TRUE, stringsAsFactors = F)
colnames(sales)

display.brewer.all()

rand.data <- replicate(8, rnorm(35, 35, sd=1.5))
boxplot(rand.data, col=brewer.pal(8, 'Set1'))

num.colors <- 8
xtz <- colorRampPalette(c('blue', 'red', 'green'))
my.cols <- xtz(num.colors)
boxplot(rand.data, col=my.cols)

# plot expense and income
plot(sales$expenses, sales$income, pch=16, cex=1, col='orange')

col.vec <- rep('orange', nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex=1, col=col.vec)

col.vec <- rep(rgb(30, 144, 255, maxColorValue = 255), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex=1, col=col.vec)

new.col.vec <- rep(rgb(144,238,144,alpha=128,maxColorValue=255), nrow(sales))
new.col.vec[sales$unit.price > 14] <- rgb(128,0,128,alpha=128,maxColorValue=255)
new.col.vec[sales$unit.price > 20] <- rgb(139,0,0,alpha=128,maxColorValue=255)
plot(sales$expenses, sales$income, pch=16, cex=1, col=new.col.vec)

# Different color for different sales/unit-price
col.vec[sales$unit.price > 14] <- rgb(255, 64, 64, maxColorValue = 255)
plot(sales$expenses, sales$income, pch=16, cex=1, col=col.vec)

# Set sales
col.vec[sales$type == 'red'] <- rgb(255, 64, 64, maxColorValue = 255)
plot(sales$expenses, sales$income, pch=16, cex=1, col=col.vec)

# Exercise
x <- rnorm(117)
y <- rnorm(117)

df <- data.frame(x=x, y=y)

df$col <- rgb(64, 64, 255, maxColorValue = 255)
# Since we are trying to set the bottom values to red use
# y-axis as the pivot for <0 
df$col[df$y <= 0] <- rgb(255, 64, 64, maxColorValue = 255)

plot(df$x, df$y, pch = 16, cex = 1, col = df$col)


# Overplotting & transparency
col.vec <- rep(rgb(0.8, 0.15, 0.15), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex=1, col=col.vec)

# Use a combination of alpha and cex to control the overlap
col.vec <- rep(rgb(0.8, 0.15, 0.15, alpha = 0.2), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex=0.4, col=col.vec)

# Ramp with 'red' being most intense
smoothScatter(sales$expenses, sales$income
              , colramp = colorRampPalette(c('white', 'cyan', 'pink', 'red')))

install.packages('aplpack')
library(aplpack)

# The * shows the central-tendency of the plot and helps fairly unclutter all
# the overlapping
bagplot(sales$expenses, sales$income
        , show.whiskers = F
        , col.loophull = '#aaccff'
        , col.looppoints = '#3355ff'
        , col.baghull = '#7799ff'
        , col.bagpoints = '#000088'
        , transparency = T)


col.vec <- rep(rgb(30, 144, 255, maxColorValue = 255, alpha = 100), nrow(sales))
col.vec[sales$unit.price > 10] <- rgb(64, 255, 64, maxColorValue = 255, alpha = 100)
col.vec[sales$unit.price > 14] <- rgb(255, 64, 64, maxColorValue = 255, alpha = 100)

plot(sales$expenses, sales$income, col=col.vec)


# Adobe Illustrator exercises
# Plot-1
n <- 1000
x <- rnorm(n)
y <- x ^ 2 + rnorm(n, mean=1, sd = 0.25)
plot(c(x, -1.5, 1.5, 0), c(y, 14, 14, 0))

# Plot-2
A <- sample(c('here', 'there', 'nowhere', 'everywhere'), size=n, replace=T)
B <- sample(c('now', 'later'), size=n, replace=T)
barplot((table(B,A)), beside = T)


# Plot-3
pie(table(A))
