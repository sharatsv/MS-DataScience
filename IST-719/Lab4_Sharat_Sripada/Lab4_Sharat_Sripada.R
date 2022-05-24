# Lab-4: Layouts in R

# Get the sales.csv
fname <- file.choose()
sales <- read.csv(fname, header=T, stringsAsFactors = F)

dat.1 <- tapply(sales$unit.price, list(sales$wine), sum)
dat.2 <- tapply(sales$income, list(sales$wine), sum)


# Plot the two data points in a bar-plot
par(mfrow = c(2,1))
par(mar = c(0.5, 5, 4, 1), cex.lab = 0.8)

barplot(dat.2, xaxt='n', las=2)  # Use xaxt to turn off the x-axis

mtext(text='income', side=2, line=4, adj=0) # side = 2 is on the left
mtext(text='Income on units sold', side=3, line=1, cex=1.3, adj=0) # side = 3 is on the top/Title

# Insert the second plot for dat.1
par(mar = c(6, 5, 0, 1), cex.lab = 0.8)
bar.out <- barplot(dat.2, xaxt='n', las=2)

axis(side=1, at = bar.out, labels=gsub(" ", "\n", names(dat.2)), las=2)

# Use layouts with R with the Matrix function. Layouts can help define 
# spacing and distance more accurately.

M <- matrix(
  c(1,1,1
    , 1,1,1
    , 2,2,2), nrow=3, byrow=T)
layout(M)
layout.show(2)

# Plot the charts in the layouts respectively
par(mar = c(0.5, 5, 4, 1), cex.lab = 0.8)
barplot(dat.2, xaxt='n', las=2)  # Use xaxt to turn off the x-axis
mtext(text='income', side=2, line=4, adj=0) # side = 2 is on the left
mtext(text='Income on units sold', side=3, line=1, cex=1.3, adj=0) # side = 3 is on the top/Title


par(mar = c(6, 5, 0, 1), cex.lab = 0.8)
bar.out <- barplot(dat.2, xaxt='n', las=2)
axis(side=1, at = bar.out, labels=gsub(" ", "\n", names(dat.2)), las=2)


# Example-2: Adding more dimensions to the layout using matrix
M <- matrix(
  c(1,1,1,3
    , 1,1,1,4
    , 3,3,3,5), nrow=3, byrow=T)
layout(M)
layout.show(5)

# Plot the charts in the layouts respectively
par(mar = c(0.5, 5, 4, 1), cex.lab = 0.8)
barplot(dat.2, xaxt='n', las=2)  # Use xaxt to turn off the x-axis
mtext(text='income', side=2, line=4, adj=0) # side = 2 is on the left
mtext(text='Income on units sold', side=3, line=1, cex=1.3, adj=0) # side = 3 is on the top/Title


par(mar = c(6, 5, 0, 1), cex.lab = 0.8)
bar.out <- barplot(dat.2, xaxt='n', las=2)
axis(side=1, at = bar.out, labels=gsub(" ", "\n", names(dat.2)), las=2)

par(mar=c(1,1,1,1))
pie(dat.1)
pie(dat.2)
dat.3 <- tapply(sales$cost, list(sales$type), sum)
pie(dat.3)


# Exercise:
M <- matrix(
  c(1,2,2,2
    , 4,2,2,2
    , 3,3,3,5), nrow=3, byrow=T)
layout(M)
layout.show(5)


# Showing value in split-screen
dat.3 <- tapply(sales$units.sold, list(sales$type), sum)
dat.4 <- tapply(sales$units.sold, list(sales$rep.region), sum)
dat.5 <- tapply(sales$units.sold, list(sales$year), sum)

split.screen(figs=c(2,1))
screen(1)
pie(dat.1)

screen(2)
pie(dat.2)

# Add titles to each screen
screen(1, new=F)
mtext('Jeff', side=3, line=1)

screen(2, new=F)
mtext('here', side=3, line=1)

close.screen(1:2)


# Fonts
n <- 500
x <- abs(rnorm(n, 6, 2))
y <- x^2 + rnorm(n, 0, 2*x)

my.par <- par()
my.par$adj 
my.par$family

plot(x,y)
my.par$font

plot(x,y,main='Fiddling with Fonts',xlab='some x lab'
     ,ylab='ylab text')

plot(x,y,main='Fiddling with Fonts',xlab='some x lab'
     ,ylab='ylab text', font=2)
plot(x,y,main='Fiddling with Fonts',xlab='some x lab'
     ,ylab='ylab text', font=3)
plot(x,y,main='Fiddling with Fonts',xlab='some x lab'
     ,ylab='ylab text', font=4)
plot(x,y,main='Fiddling with Fonts',xlab='some x lab'
     ,ylab='ylab text', font=5)


# Play with font axis and front label
my.par$font.axis
my.par$font.lab

plot(x,y,main='Fiddling with Fonts',xlab='some x lab'
     ,ylab='ylab text', font.axis=2, font.lab=3, font.main=1)

# Set the family
plot(x,y,main='Fiddling with Fonts',xlab='some x lab'
     ,ylab='ylab text', family='HersheyGothicEnglish')

# ANother way to do this with par
par(family='mono')
plot(x,y,main='Fiddling with Fonts',xlab='some x lab'
     ,ylab='ylab text')

par(family='seriff')
plot(x,y,main='Fiddling with Fonts',xlab='some x lab'
     ,ylab='ylab text')

par(family='sans')
plot(x,y,main='Fiddling with Fonts',xlab='some x lab'
     ,ylab='ylab text')







