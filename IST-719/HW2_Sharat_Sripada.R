# HW-2

# Part-1: Charts from Visualize This
fname <- '/Users/venkatasharatsripada/Documents/Masters@Syracuse/Course-Related(Study)/IST-719/hot-dog-contest-winners.csv'
hotdogs <- read.csv(fname, sep=',', header=T)

# Examine the data
str(hotdogs)
dim(hotdogs)

# Barplot: Figure 4-11
# Multiple colors with barplot. Here we intend to highlight years that 
# United States won the contest

fill_colors <- c()
for (i in 1:length(hotdogs$Country)) {
  if (hotdogs$Country[i] == 'United States') {
    fill_colors <- c(fill_colors, "#821122") #color - Dark red
  } else{
    fill_colors <- c(fill_colors, "#cccccc") #color - Grey
    
  }
}

dev.new()
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors,
        border=NA, xlab="Year", ylab="Hot dogs and buns (HDB) eaten")

# Export with size 3 x 5 in (+ Portrait)

# Stacked Bar chart: Figure 4-22
fname <- '/Users/venkatasharatsripada/Documents/Masters@Syracuse/Course-Related(Study)/IST-719/hot-dog-places.csv'
hot_dog_places <- read.csv(fname, sep=',', header=T)

# Examining the data we see R place an X before each year. Modify that first
names(hot_dog_places) <- c(2000:2010)

# Convert to matrix to stack barplot
hot_dog_matrix <- as.matrix(hot_dog_places)

barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0, 200),
        xlab="Year", ylab="Hot dogs and buns (HDBs) eaten",
        main="Hot Dog Eating Contest Results, 1980-2010")


# Scatter plot: Figure 4-28
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv",
           sep=",", header=TRUE)

# Note type=h here for vertical lines
plot(subscribers$Subscribers, type="h", ylim=c(0, 30000),
     xlab="Day", ylab="Subscribers")

# On the same plot highlight the points with pch and fill with Black color
points(subscribers$Subscribers, pch=19, col="black")


# Time-series: Figure 4-34

population <- read.csv("http://datasets.flowingdata.com/world-population.csv",
           sep=",", header=TRUE)

# Simple time-series plot
plot(population$Year, population$Population, type="l",
     ylim=c(0, 7000000000), xlab="Year", bty='n', ylab="Population")


# Step-chart: Figure 4-43
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv",
                    sep=",", header=TRUE)

# Simple step chart with type='s'
plot(postage$Year, postage$Price, type="s",
     main="US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab="Year", ylab="Postage Rate (Dollars)", cex.main=0.9)


# Part-2: Simple Distributions
art <- read.csv('/Users/venkatasharatsripada/Documents/Masters@Syracuse/Course-Related(Study)/IST-719/art.csv')

# See head of data-frame
head(art)

# See size of data
dim(art)

# Split the chart area using par() into 2x2 grid
dev.new()
par(mfrow = c(2,2))

# Plot distribution of total sales
# Since the number of points are high (10k), let's see the density of the plot
d <- density(art$total.sale)
plot(d, main = 'Distribution of total.sales (density)')
polygon(d, col='orange')

# The plot shows a significant drop in total sales in recent years

# Next, let's use box-plot to visualize and gain 
boxplot(art$total.sale, horizontal = T, xlab = 'Total Sales', 
        main = 'Distribution of total.sales')

# Subsets of total sales
# Plot sales of Drawing paper
draw <- art[art$paper == 'drawing', ]
dim(draw)

# Since the number of points are large (>4k), let's group by year using tapply()
D <- tapply(draw$total.sale, list(draw$year), sum)
barplot(D, ylab='Total Sales', xlab='Year', col='#cccccc', main='Distribution of total sales for drawing paper'
        , cex.main=0.9, ylim=c(0,30000))

# Plot sales of watercolor paper
wcolor <- art[art$paper == 'watercolor', ]
dim(wcolor)

W <- tapply(wcolor$total.sale, list(wcolor$year), sum)
barplot(W, ylab='Total Sales', xlab='Year', col='blue', main='Distribution of total sales for watercolor paper'
        , cex.main=0.9, ylim=c(0,30000))


# Part-3: 
art <- read.csv('/Users/venkatasharatsripada/Documents/Masters@Syracuse/Course-Related(Study)/IST-719/art.csv'
                , stringsAsFactors = F)

dev.new()
par(mfrow = c(2,2))
# Correlation between unit.price and units.sold

# Before we consolidate or group let's see if the unit.price is categorical
# str(art) shows it's type num

unit_price <- art$unit.price
unique(unit_price)

# unique() show 8-categories in all
# 12.15 20.99 10.26 24.94  0.36 19.34  0.77 97.99

# Let us group by unit price and calculate the sum of sales per unit price category
unit_price.grouped <- tapply(art$units.sold, list(sort(art$unit.price), decreasing=T), sum)
barplot(unit_price.grouped, main='Units sold by unit.price category', xlab='Unit.price'
        ,ylab='Units sold', col='orange')

# This shows that the highest sold units are in the 0.77 unit.price category. Ideally, we 
# would expect an increasing or decreasing order but there seems no correlation

# Sell more of drawing paper or watercolor paper
draw <- art[art$paper == 'drawing', ]
draw_units.sold <- sum(draw$units.sold)

wcolor <- art[art$paper == 'watercolor', ]
wcolor_units.sold <- sum(wcolor$units.sold)

barplot(c(draw_units.sold, wcolor_units.sold), names.arg = c('Drawing', 'Watercolor')
        , col = c('#cccccc', 'blue'), main='Comparison of units sold (Draw vs Watercolor)', ylab='Units sold'
        , cex.main = 0.95)

# Which product has more sales 
draw_total.price <- sum(draw$total.sale)
wcolor_total.price <- sum(wcolor$total.sale)

barplot(c(draw_total.price, wcolor_total.price), names.arg = c('Drawing', 'Watercolor')
        , col = c('#cccccc', 'blue'), main='Comparison of total sales (Draw vs Watercolor)', ylab='Total sales'
        , cex.main=0.95, ylim=c(0, 120000))


