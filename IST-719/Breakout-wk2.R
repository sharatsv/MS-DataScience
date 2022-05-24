fname <- file.choose()

wine <- read.csv(fname, sep='\t', stringsAsFactors = T)
str(wine)
head(wine)
colnames(wine)

dev.new()
par(mfrow = c(2,2))

# Part-1
wine_filter <- tapply(wine$units.sold, list(wine$type, wine$rep.region), sum)
barplot(wine_filter, beside=T, col=c('red', 'white'))

# Part-2
wine_filter2 <- tapply(wine$units.sold, list(wine$rep.region, wine$type), sum)
barplot(wine_filter2, beside=T, col=c('green', 'blue', 'orange', 'red', 'white'))


# Question: Highest income wine for each region in 2012
str(wine)
wine_2012 <- wine[wine$year == 2012, ]
wine_2012_G <- tapply(wine_2012$income, list(wine_2012$rep.region, wine_2012$wine), sum)
barplot(wine_2012_G, beside=T)
class(wine_2012_G)


# Question: sales representatives who sold the most units of white wine in each region in 2010
wine_2010 <- wine[wine$year == 2010, ]
wine_2010_white <- wine_2010[wine_2010$type == 'white', ]

wine_2010_G <- tapply(wine_2010_white$units.sold, list(wine_2010_white$sales.rep), sum)
par(mar = c(4, 9, 2, 4))
barplot(sort(wine_2010_G, decreasing = F), beside=T, horiz = T, las=1)
