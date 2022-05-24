# Lab-2 - Week 2
# Data interrogation or data exploration and distribution 

# fname <- file.choose()
fname <- '/Users/venkatasharatsripada/Downloads/tips.csv'

# To read from csv file
tips <- read.csv(file = fname
                 , header = TRUE
                 , stringsAsFactors = FALSE)

# View the column names of the data in tips 
colnames(tips)  

# To edit the data use fix()
fix(tips)

# Simpler view of the data use view()
# install.packages('ctv')
view(tips)

# View the structure of the data
str(tips)

# Accessing the data
# Get 1st row
tips[1,]

# Get 1st col
tips[, 1]

# Get a specific value from a position (3,3)
tips[3, 3]

# Get first 3-rows
tips[1:3, ]

# Get the size of data
length(tips[1:3, 2])

# Get the dimension
dim(tips)

# Get the columns using indexing
dim(tips)[1]

# Plot the column total_bill
plot(tips$total_bill)

# Sort the data and plot it
plot(sort(tips$total_bill))

# Make a box plot of the column total_bill
boxplot(tips$total_bill)

# Make a histogram of the column total_bill
hist(tips$total_bill)


# View the density/population of column total_bill
d <- density(tips$total_bill)
plot(d)


# Visualizing the distribution of the tip column
plot(tips$tip)
boxplot(tips$tip)
hist(tips$tip)
d <- density(tips$tip)
plot(d)


# Use par() to create a grid-like graph/chart layout
par(mfrow = c(2,2))
boxplot(tips$tip)
hist(tips$tip)
d <- density(tips$tip)
plot(d)
polygon(d, col='orange')
library(vioplot)
vioplot(tips$tip)

# Subsets - Figuring if a certain sex tips more 
tips.M <- tips[tips$sex == 'Male', ]
tips.F <- tips[tips$sex == 'Female', ]

par(mfrow = c(2,1), mar = c(2,3,1,2))
# Use xlim/ylim to normalize the axes across the two plots
boxplot(tips.F$tip, horizontal = T, ylim = c(1, 10))
boxplot(tips.M$tip, horizontal = T, ylim = c(1, 10))

# Result: This visualization shows that Males tend to tip more than Females.


# Working with JSON files
# fname <- file.choose()
fname <- '/Users/venkatasharatsripada/Documents/Masters@Syracuse/Course-Related(Study)/IST-719/tweet.formated.json'

library(jsonlite)

raw.tweet <- fromJSON(fname, flatten=FALSE)
str(raw.tweet)

# Use names to see column names/keys
names(raw.tweet)

# Fetch some values
raw.tweet$text
raw.tweet$user$followers_count

# Here's another way to pull data from JSON using stream_in. 
# fname <- file.choose()

fname <- '/Users/venkatasharatsripada/Downloads/tweets5814.json'

con <- file(fname, 'r')
tweets <- stream_in(con)
close(con)

dim(tweets)

tweets$text[1:3]

# Boxplot the number of followers per user 
boxplot(log10(tweets$user$followers_count), horizontal = TRUE)


# Custom data
task.time <- c(rnorm(n = 30, mean = 30, sd = 2.25), rnorm(n = 30, mean = 25, sd = 1.5))
# Use replicate to create values 'AMA' and 'PRO' - 30x
status <- c(rep('AMA', 30), rep('PRO', 30))

# Create a dataframe
df <- data.frame(time = task.time, status = status)

str(df)

# Grouping using the aggregate function 
df.grouped <- aggregate(df$time, list(df$status), mean)
colnames(df.grouped) <- c('stat', 'time')
df.grouped

# To view the type
class(df.grouped)

barplot(df.grouped$time, names.arg = df.grouped$stat)

# Use tapply for grouping
# tapply is similar to aggregate - only diff is tapply returns array vs aggregate returns a df
M.grouped <- tapply(df$time, list(df$status), mean)

# Add a new column sex to the df and run grouping functions
df$sex <- sample(c('M', 'F'), 60, replace = T)

df.grouped <- aggregate(df$time, list(df$status, df$sex), mean)

# tapply is a little more useful since it makes a nice grid
M <- tapply(df$time, list(df$status, df$sex), mean)

M <- tapply(df$time, list(df$sex, df$status), mean)

barplot(M, beside = T)


# Reshaping data with tidyr
library(tidyr)
n <-5 
year <- 2001:(2000 + n)
q1 <- runif(n = n, min = 100, max = 120)
q2 <- runif(n = n, min = 103, max = 130)
q3 <- runif(n = n, min = 105, max = 140)
q4 <- runif(n = n, min = 108, max = 150)

df.wide <- data.frame(year, q1, q2, q3, q4)

# Use gather() to transpose data from wide -> long
# Gather by quarters
df.long <- gather(df.wide, qt, sales, q1:q4)

# Alternate method to skip providing df.wide as argument
# df.long <- df.wide %>% gather(qt, sales, q1:q4)

# Sort the data
o <- order(df.long$year, df.long$qt)
df.long <- df.long[o, ]


# Create a cust data-set to transpose from long to wide using spread()
df <- data.frame(cat = rep(c('tap', 'reg', 'zed', 'vum'), 3)
                 , group = rep(letters[7:9], 4)
                 , x = 1:12)


spread(df, cat, x)


# Custom plots
install.packages("plotrix")
library(plotrix)
n <- 7000
age.min <- 1
age.max <- 90
age.range <- c(age.min, age.max)
m <- round(rescale(rbeta(n, 5, 2.5), age.range), 0)
f <- round(rescale(rbeta(n, 5, 2.0), age.range), 0)
x <- age.min:age.max
f.y <- m.y <- rep(0, length(x))

m.tab <- table(m)
m.y[as.numeric((names(m.tab)))] <- as.numeric(m.tab)


f.tab <- table(f)
f.y[as.numeric((names(f.tab)))] <- as.numeric(f.tab)

age.freqs <- data.frame(ages=x, males=m.y, females=f.y)

max.x <- round(1.2 * max(age.freqs[, 2:3]), 0)

# Create a plot space. type='n' is No-Plot
plot(c(-max.x, max.x), c(0,100), type='n', bty='n', xaxt='n'
     , ylab='age', xlab='freq', main='sample age distribution')

grid()
last.y <- 0
for (i in 1:90) {
  rect(xleft = 0, ybottom = last.y, xright = -age.freqs$males[i]
       ,ytop = age.freqs$ages[i], col="lightblue2", border=NA)
  rect(xleft = 0, ybottom = last.y, xright = age.freqs$females[i]
       ,ytop = age.freqs$ages[i], col="lightpink", border=NA)
  last.y <- age.freqs$ages[i]
  
  }