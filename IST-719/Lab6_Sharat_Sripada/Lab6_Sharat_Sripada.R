# Lab6 - ggplot

fname <- file.choose()
sales <- read.csv(fname, header=T, stringsAsFactors = F)

library(ggplot2)

p <- ggplot(sales)
p
class(p)
attributes(p)

# Copies the whole data into data attribute so the 
# sales data exists in memory twice:
# - sales (df) & p (ggplot var)
p$data
p$layers
p$scales
summary(p)

ggplot(sales) + aes(x = expenses)
range(sales$expenses)

# Essentially, aesthetics and geometric object constitute ggplot
# NOTE: We cannot directly call the columns from the data-frame like:
# sales$expenses or sales$income

ggplot(sales) + aes(x = expenses, y = income) + geom_point()

# Different ways of doing the plot
p <- ggplot(sales)
p + aes(x = expenses, y = income) + geom_point()

# Add some color to the plot
p <- ggplot(sales) + aes(x = expenses, y = income)
p + geom_point(color='blue')

# Color the plot based on the type (red/white wine)
ggplot(sales) + aes(x = expenses, y = income, color = type) + geom_point()



# Aesthetic setting vs mapping
ggplot(sales) + aes(x = expenses, y = income, color = unit.price > 14) + geom_point()

# Now do the same using aesthetic setting
ggplot(sales) + aes(x = expenses, y = income) + geom_point(color = ifelse(sales$unit.price > 14, 'red', 'green'))


# Adding shape 
ggplot(sales) + aes(x = expenses, y = income, color = unit.price, shape = type) + geom_point()

# Adding more attributes like alpha, size
ggplot(sales) + aes(x = expenses, y = income
                    , color = rep.region
                    , shape = type
                    , alpha = unit.price
                    , size = units.sold) + geom_point()


# Exercise:
# Using gglot set the color of the points
ggplot(sales) + aes(x = expenses, y = income) + geom_point(color='red')

# Using gglot set the color of points to some variable
ggplot(sales) + aes(x = expenses, y = income, color = type) + geom_point()


ggplot(sales) + aes(x = expenses, y = income) + geom_point() + geom_rug()

# Make a prediction using a linear model & add a trendline to the plot
income.predict <- predict(lm(sales$income~sales$expenses))
ggplot(sales) + aes(x = expenses, y = income) + geom_point() + 
  geom_line(aes(y = income.predict), color='red', lwd=3)

# Note adding plot on top of each other - stacking plots
ggplot(sales) + aes(x = expenses, y = income) + geom_point(color='pink') +
  geom_rug() + 
  geom_line(aes(y = income.predict)) +
  geom_line(aes(y = income.predict + 150)) + 
  geom_vline(xintercept = 10, color='blue') + 
  geom_hline(yintercept = 500, color='orange') + 
  geom_abline(intercept = 50, slope = 100, color='red',lty=3,lwd=2)

# Smooth plots
ggplot(sales) + aes(x = expenses, y = income) + geom_point() +
  geom_smooth(method='loess')

# Denisty plot
ggplot(sales) + aes(x = expenses, y = income) + geom_bin2d(bins=50)

price <- ifelse(sales$unit.price > 14, 'expensive', 'moderate')
price[sales$unit.price < 9] <- 'cheap'

ggplot(sales) + aes(x = expenses, y = income, color = price) + 
  geom_bin2d(bins=50)



df <- aggregate(sales$units.sold, list(year=sales$year), sum)
df2 <- aggregate(sales$units.sold, 
                 list(year=sales$year, region=sales$rep.region), sum)

ggplot(sales) + aes(x=income) + 
  geom_histogram(binwidth = 10, fill='orange') + 
  geom_vline(aes(xintercept = mean(income))
             ,color='blue', linetype='dashed', size=1)

# Stats
ggplot(sales) + aes(x=income) + 
  geom_histogram(binwidth = 10, fill='orange', alpha=0.9) + 
  aes(y=..density..) + 
  geom_density(alpha=0.3, fill='blue', color='blue')

# boxplot
ggplot(sales) + aes(x='jeff', y=income) +
  geom_boxplot()

ggplot(sales) + aes(x=rep.region, y=income) +
  geom_boxplot()

# Line plot
ggplot(df) + aes(x=year, y=x) + geom_line() + ylim(c(0,40000))

# Step function
ggplot(df) + aes(x=year, y=x) + geom_step() + ylim(c(0,40000))

# Ribbon - Gives you a highlight around the line based on a defined range
ggplot(df) + aes(x=year, y=x) + 
  geom_ribbon(aes(ymin = x - 1000), ymax = df$x + 1000, fill='yellow') + 
  geom_line() + ylim(c(0,40000))

# 
ggplot(df2) + aes(x=year, y=x, color=region) + geom_line() + ylim(c(0,10000))

# Exercise:
df2$angle <- runif(25, 0, 2*pi)
df2$xnew <- runif(25, 0, sqrt(0.1 * df2$x))
ggplot(df2) + aes(x=year, y=x, color=region) + geom_spoke(aes(angle=angle, radius=xnew))


# Moving to bar plots
df <- aggregate(sales$units.sold, list(region=sales$rep.region), sum)
colnames(df)[2] <- 'sales'

ggplot(sales) + aes(x=rep.region) + geom_bar(fill='orange', width=.5) + 
  ggtitle('Number of sales by region')


ggplot(sales) + aes(x=rep.region, fill=type) + 
  geom_bar(position='fill')


ggplot(df) + aes(x=region, y=sales, fill=region) + geom_bar(stat='identity')  


# pie-chart or circular bar chart
ggplot(df) + aes(x='', y=sales, fill=region) + 
  geom_bar(width=0.3, stat='identity') + 
  coord_polar('y', start=45)


# Playing with stats
p <- ggplot(sales) + aes(x=income)
p + geom_histogram() + stat_bin(binwidth = 20)
p + stat_density()

# stat_boxplot() is same as geom_boxplot()
ggplot(sales) + aes(y=income) + stat_boxplot()

# 
ggplot(sales) + aes(x=expenses, y=income) + stat_bin2d() +
  stat_density_2d(col='red')


ggplot(sales) + aes(x=rep.region) + geom_bar()

ggplot(sales) + aes(x=rep.region) + stat_count()


# More stats..
# See ..count.. and ..density.. stat functions called
ggplot(sales) + aes(x=income) + 
  geom_histogram(aes(fill=..count..)) +
  aes(y=..density..) + 
  geom_density(fill = 'yellow', alpha=0.1)


# Memory commands
object.size(p)

memory.size()

# Garbage collector
gc()

# To remove objects from memory
rm(<object>)

# Generally you can run rm -> gc in that cycle 

