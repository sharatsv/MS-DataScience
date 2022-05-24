# Week-4 Breakout

fname <- file.choose()
art <- read.csv(fname, header = T, stringsAsFactors = F)

head(art)
colnames(art)
par(mfrow = c(3,2))

# Plot-1
plot(art$unit.price, art$units.sold, pch=16, cex=1, col='orange')

# Plot-2
M <- tapply(art$total.sale, list(art$year), sum)
barplot(M, main='Sales by year', col='red')

# Plot-3
boxplot(art$units.sold)

# Plot-4
N <- tapply(art$units.sold, list(art$paper.type), sum)
pie(N)

# Plot-5
pie(N)

# Plot-6
pie(N)


# Exercise-2
M <- matrix(
  c(1,2,2
    , 4,2,2
    , 3,3,5), nrow=3, byrow=T)
layout(M)
layout.show(5)

par(mar = c(0.5, 5, 4, 1), cex.lab = 0.8)


# Exercise-3
par(mfrow = c(2, 2))
plot(art$unit.price, art$units.sold, pch=16, cex=1, col='orange'
     , main='Exercise-3', font=2)

plot(art$unit.price, art$units.sold, pch=16, cex=1, col='red'
     , font.lab=3, main='Exercise-3', font.main=1)

plot(art$unit.price, pch=16, cex=1, art$units.sold, main='Fiddling with Fonts',xlab='some x lab'
,ylab='ylab text', family='HersheyGothicEnglish')



