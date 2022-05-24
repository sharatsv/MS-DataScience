# Week-5 Lab: Twitter data

fname <- '/Users/venkatasharatsripada/Documents/Masters@Syracuse/Course-Related(Study)/IST-719/climatetweets_useforlecture_25k.csv'
tweets <- read.csv(fname, header=T, quote="\"", stringsAsFactors = F)

str(tweets)

my.media <- tweets$media

# To view counts of each category
table(my.media)

# Replace blank with text only
my.media[my.media == ''] <- 'text only'

# Use gsub to substitute photo|photo with photo
my.media <- gsub('\\|photo', '', my.media)

# Convert to percentages & plot a pie
pie(100 * round(table(my.media)/sum(table(my.media)), 4))

tweets$created_at[1:3]

# Sample created at format:
# Mon Aug 15 13:05:42 +0000 2016
conversion.string  <- '%a %b %d %H:%M:%S +0000 %Y'

tmp <- strptime(tweets$created_at, conversion.string)

# Class-type is POSIXt/POSIXlt
class(tmp)

# Check if any NA in tmp
any(is.na(tmp))

# Remove tmp after experiment
rm(tmp)

# Experiment with how to extract key params from date/time format
tmp <- '10AM and 27 minutes, on Jun 22, 1999'
# Use strptime to fetch conversion str
str
strptime(tmp, '%H%p and %M minutes, on %B %d, %Y')

tweets$date <- strptime(tweets$created_at, conversion.string)

min(tweets$date)
max(tweets$date)
range(tweets$date)
summary(tweets$date)
difftime(min(tweets$date), max(tweets$date), units='min')
difftime(min(tweets$date), max(tweets$date), units='weeks')

# Use the lubridate library
library(lubridate)

# Extract Day from date column
wday(tweets$date, label=T, abbr=T)

# Count using table
table(wday(tweets$date, label=T, abbr=T))

# Finally, use the bar-plot to plot this
barplot(table(wday(tweets$date, label=T, abbr=T)))

# What time of day are ppl sending out most tweets

# First, let's correct the time-zone using the user_utc_offset column
tmp <- tweets$user_utc_offset
tweets$date[1:3]
tweets$date[1:3] + tmp[1:3]

# Patch the column and pull it into known.times
known.times <- tweets$date + tmp

# Remove all rows that have NA if it exists
any(is.na(known.times))

index <- which(is.na(known.times))
known.times <- known.times[-index]

# Get hour from known.times using hour() 
table(hour(known.times))

barplot(table(hour(known.times)))

barplot(known.times)

# Exercise - 5.1.4
p <- "2018.08.30-16.24.49"
strptime(p, '%Y.%m.%d-%H.%M.%S')


start.date <- as.POSIXct('2016-06-25 00:00:00')
end.date <- as.POSIXct('2016-06-25 23:59:59')

index <- which((tweets$date > start.date) & (tweets$date < end.date))

tweets.25th <- tweets$date[index]

format.Date(tweets.25th, '%Y%m%d%H%M')
tmp.date <- as.POSIXct(strptime(format.Date(tweets.25th, '%Y%m%d%H%M'), 
                                '%Y%m%d%H%M'))
tmp.tab <- table(tmp.date)

x <- seq.POSIXt(from=start.date, to=end.date, by='min')

length(x)
# Length now matches 24 * 60 = 1440

y <- rep(0, length(x))

y[match(names(tmp.tab), as.character(x))] <- as.numeric(tmp.tab)

plot(x, y, type='p', pch=16, cex=0.4)

# Use the seq.POSIXct() for Exercise 5.1.6
start.date <- as.POSIXct('2022-01-01')
end.date <- as.POSIXct('2022-03-31')

seq.POSIXt(from=start.date, to=end.date, by='day')

# Hashtag wordcloud
library(stringr)
tags <- str_extract_all(tweets$text, '#\\S+', simplify =FALSE)

# Using the lengths function remove all instances where tweets had no tags
tags <- tags[lengths(tags) > 0]

# Convert to vector
tags <- unlist(tags)

# Normalize the text with removing all the captial letters etc.
tags <- tolower(tags)

tags <- gsub('#|[[:punct:]]', '', tags)

# Visualize in table format
tags.tab <- sort(table(tags), decreasing = T)

tags.tab[1:10]

# Remove noise
zap <- which(tags.tab < 3)
tags.tab <- tags.tab[-zap]

plot(as.numeric(tags.tab))

# The plot is highly skewed so we transform the data
df <- data.frame(words = names(tags.tab), count = as.numeric(tags.tab), stringsAsFactors = F)

par(mfrow = c(3,3))
plot(df$count, main='raw')

y <- df$count / max(df$count)
plot(y, main = '0-1')

# Square
plot(df$count ^ 2, main = '^2')

# Root
plot(df$count ^ (1/2), main = '^(1/2)')

# Fifth-root
plot(df$count ^ (1/5), main = '^(1/5)')

# log base 10 transformations
plot(log10(df$count), main = 'log10')
plot(log10(df$count), main = 'log10')
plot(log(df$count), main = 'log10')

# Make a wordcloud
library(wordcloud)
myPal <- colorRampPalette(c('red', 'orange', 'gold')) 

# Clean R's memory using the garbage collection function
gc()

# Filter out words with low freq
index <- which(df$count > 9)
par(mar=c(0,0,0,0), bg='black')
my.counts <- (df$count[index])^1/2
wordcloud(df$words[index], my.counts, scale=c(4, 0.4), min.freq = 1
          , max.words = Inf, random.order = F, random.color = F
          , ordered.colors = T
          ,rot.per = 0, colors = myPal(length(df$words[index])))


# Visualizing via Alluvial plot
fname <- file.choose()
sales <- read.csv(fname, header = T, stringsAsFactors = F)

library(alluvial)

# Visualize the Titanic data-set through an Alluvial plot
dat <- as.data.frame(Titanic, stringsAsFactors = F)
alluvial(dat[,1:4], freq=dat$Freq)

# Improve alluvial visualization with a sales.csv plot

alluv.df <- aggregate(sales$units.sold
                      , list(sales$rep.region, sales$type)
                      , sum)
colnames(alluv.df) <- c('reg', 'type', 'units.sold')
alluvial(alluv.df[,1:2], freq=alluv.df$units.sold)

# Add some color 
my.cols <- rep('gold', nrow(alluv.df))
my.cols[alluv.df$type == 'red'] <- 'red'
alluvial(alluv.df[,1:2], freq=alluv.df$units.sold
         , col=my.cols)

# Using ifelse for colors
alluvial(alluv.df[,1:2], freq=alluv.df$units.sold
         , col=ifelse(alluv.df$type=='red', 'red', 'gold'))


# Now, add a new dimension to the alluv.df, adding the wine as a category
alluv.df <- aggregate(sales$units.sold
                      , list(sales$rep.region, sales$type, sales$wine)
                      , sum)
colnames(alluv.df) <- c('reg', 'type', 'wine', 'units.sold')
alluvial(alluv.df[,1:3], freq=alluv.df$units.sold
         , col=ifelse(alluv.df$type=='red', 'red', 'gold')
         , border=1)


# Moving onto tree plots
library(RColorBrewer)
library(treemap)

# treemap does the aggregate function for us
treemap(sales, index = c('rep.region')
        ,vSize = 'income'
        ,vColor = 'units.sold'
        ,type = 'value'
        ,fontsize.labels = 18
        ,palette = 'OrRd')








