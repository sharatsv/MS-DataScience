# LiveClass-2 (Breakout session)

# Question-1
aa<-sample(1:5)
bb<-sample(1:5)
cc<-sample(1:5)
df<-data.frame(aa,bb,cc)
df
# Use rowMeans to calculate mean on every row
# Usage:
# rowMeans(x, na.rm = FALSE, dims = 1, \dots)
df$rm <- rowMeans(df)
maxmean <- which.max(df$rm)
df[maxmean,]


# Question-2
mycars <- mtcars
# NOTE: which.max() doesn't get all the rows with 8x cylinders
maxcyl_index <- which.max(mycars$cyl)
maxcyl <- mycars[maxcyl_index,]

# Here's an alternate to check/list all the cars with 
mycars[mycars$cyl==8,]

