# HW-4 - Chapter-6 - Visualizing relationships

# Load the data
crime <- read.csv('http://datasets.flowingdata.com/crimeRatesByState2005.csv'
                  , sep=",", header=TRUE)

# Examine the data
str(crime)
colnames(crime)
head(crime)

# Remove Washington DC and USA and place full focus on individual states
crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime2$state != "United States",]

# Creating a scatterplot matrix using plot()
plot(crime2[, 2:9])

# Use pairs() to create a scatter plot with fitted LOESS curves
# What are LOESS curves:
# Slopes are straight lines and can't be used to fit on winds and curves. Instead,
# we use LOESS which is a locally weighted scatterplot smoothing technique. LOESS
# starts at the beginning of the data and takes small slices. At each  slice it
# estimates a low-degree polynomial for just the data in the slice. LOESS moves
# along the data, fitting a bunch of tiny curves, eventually forming a single curve.
pairs(crime2[,2:9], panel=panel.smooth)


# Creating a bubble plot showing crime in USA
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv",
          header=TRUE, sep="\t")

# Sample plot
symbols(crime$murder, crime$burglary, circles=crime$population)

# Adjust the radius of the circle based on the area/population
radius <- sqrt( crime$population/ pi )
symbols(crime$murder, crime$burglary, circles=radius)

# Update the stroke color and fill color using fg() and bg() + update labels
# Also, using inches() reduce the size of the largest circle
symbols(crime$murder, crime$burglary, circles=radius, inches=0.35,
        fg="white", bg="red", xlab="Murder Rate", ylab="Burglary Rate")

# Add labels to the circles
text(crime$murder, crime$burglary, crime$state, cex=0.5)


# Plotting a histogram
birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
hist(birth$X2008, breaks=10, col='purple')

# Plotting a density curve
birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)
d2008frame <- data.frame(d2008$x, d2008$y)

plot(d2008)

# Use plot with polygon to fill the area under the curve with a solid color
plot(d2008, type="n")
polygon(d2008, col="#821122", border="#cccccc")

# Continued to use the birth data-set in the VT - Chapter-6 for the histogram plot
birth_yearly <-
  read.csv("http://datasets.flowingdata.com/birth-rate-yearly.csv")

str(birth_yearly)
summary(birth_yearly)

library(lattice)
histogram(~ rate | year, data=birth_yearly, layout=c(10,5))

# Remove the outlier data since 132 seems off
birth_yearly.new <- birth_yearly[birth_yearly$rate < 132,]

birth_yearly.new$year <- as.character(birth_yearly.new$year)

# Finally, update the labels with year on individual plots
h <- histogram(~ rate | year, data=birth_yearly.new, layout=c(10,5))
update(h, index.cond=list(c(41:50, 31:40, 21:30, 11:20, 1:10)))