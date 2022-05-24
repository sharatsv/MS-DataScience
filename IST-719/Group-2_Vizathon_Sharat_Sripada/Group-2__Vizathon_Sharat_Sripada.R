# Viz-a-thon
# Team: dthoma05@syr.edu, Chduncan@syr.edu, Ssharat@syr.edu

library(RColorBrewer)

df <- read.csv('/Users/venkatasharatsripada/Downloads/Vizathon_material/BuoyData.csv', stringsAsFactors = F)

# Check for any na values
print(length(which(is.na(df))))

# Replace all NA values with zero
df[is.na(df)] <- 0

head(df)
colnames(df)

# Questions:
# How have water properties changed with time? 
#   - Visualize via a time-series water quality through level of saltiness, acidity, coudiness, dissolved oxygen, 
#     algae and chlorophyll
# How has weather changed with time?
#   - Visualize via a time-series weather conditions like precipitation and wind speeds
# How does water quality vary with depth and time? 
# 


# Using strptime() let's obtain the date range
class(df$DATE_TIME) # type chr
# Sample string of date-time:
# "11/19/18 11:29" 
conversion.string <- '%m/%d/%y %H:%M'
df$DATE_TIME_POS <- strptime(df$DATE_TIME, conversion.string)
class(df$DATE_TIME_POS)
min(df$DATE_TIME_POS) # 2016-03-30 13:18:00 
max(df$DATE_TIME_POS) # 2018-11-19 11:29:00 

# Plot can get noisy with time-series - so filtering for depth = 1m, 6m, 12m, 18m
par(mfrow = c(3,2))
depths <- c(1,4,8,11,14,18)
my.col <- c('red', 'gray45', 'blue', 'orange', 'purple', '#D4FBB0')
index <- 0
for (depth in depths){
  par(mar=c(3,5,3,2))
  df.tmp <- df[df$DEPTH_m == depth, ]
  plot(df.tmp$DATE_TIME_POS, df.tmp$T_DEGC, type='l', main=paste0('Water attributes at ', depth, 'm')
       , ylab='Attributes', xlab=NA
       , col=my.col[1]
       , ylim=c(0,50)
       )
  lines(df.tmp$DATE_TIME_POS, log(df.tmp$SC_us_cm), type='l', col=my.col[2])
  lines(df.tmp$DATE_TIME_POS, df.tmp$pH, type='l', col=my.col[3])
  lines(df.tmp$DATE_TIME_POS, df.tmp$Dox_mg_L, type='l', col=my.col[4])
  lines(df.tmp$DATE_TIME_POS, df.tmp$Tn_Ntu, type='l', col=my.col[5])
  lines(df.tmp$DATE_TIME_POS, df.tmp$Chl_ug_L, type='l', col=my.col[6])
  legend(x='topright', legend=c('Temp', 'salt(log)', 'pH', 'o2', 'coudiness', 'algae'), fill=my.col, cex=0.8)
}

# Conclusion:
# As depth increases, most attribute measurements diminish. Temperature and o2 show seasonality at all depths.


# Repeat the visualization for weather
# Plot can get noisy with time-series - so filtering for depth = 1m, 6m, 12m, 18m
my.col <- c('red', 'gray45', 'blue', 'orange')
df.tmp <- df[df$DEPTH_m == 1, ]
plot(df.tmp$DATE_TIME_POS, df.tmp$PRECIP_in, type='l', main=paste0('Weather changes with time')
     , ylab='Attributes', xlab=NA
     , col=my.col[1]
     , ylim=c(0,400)
    )
lines(df.tmp$DATE_TIME_POS, df.tmp$AWND_mph, type='l', col=my.col[2])
lines(df.tmp$DATE_TIME_POS, df.tmp$WDF5_deg, type='l', col=my.col[3])
lines(df.tmp$DATE_TIME_POS, df.tmp$WSF5_mph, type='l', col=my.col[4])
legend(x='topright', legend=c('Precip', 'Wind-speed', 'Wind-dir(5s)', 'Wind-speed(5s)'), fill=my.col, cex=0.8)


# 

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))[1:18]
par(mfrow=c(2,3))
for (index in 3:8){
  attrib <- colnames(df)[index]
  if (attrib == 'SC_us_cm') {
    t <- tapply(log(df[ ,index]), list(df$DEPTH_m, df$DATE_TIME_POS$year + 1900), median)
  }
  else {
      t <- tapply(df[,index], list(df$DEPTH_m, df$DATE_TIME_POS$year + 1900), median)
  }
  barplot(t, beside = T, ylim=c(0,20), ylab=attrib, main=paste('Mean ', attrib, 'gradient (by depth)')
          , col=col_vector)
  # legend(x='topright', legend=c(1:18), fill=col_vector, cex=0.8)
}

# Repeat the same for weather
df.tmp <- df[df$DEPTH_m == 1, ]  # Since depth has no significance
par(mfrow=c(2,2))
for (index in 9:12){
  attrib <- colnames(df)[index]
  if (attrib == 'WDF5_deg') {
    t <- tapply(log(df[ ,index]), list(df$DATE_TIME_POS$year + 1900), median)
  }
  else {
    t <- tapply(df[,index], list(df$DATE_TIME_POS$year + 1900), median)
  }
  barplot(t, beside = T, ylim=c(0,30), ylab=attrib, main=paste('Mean ', attrib, 'gradient across years')
          , col=col_vector)
  # legend(x='topright', legend=c(1:18), fill=col_vector, cex=0.8)
}


# Precipation across months of the year
df.tmp$month <- format(df.tmp$DATE_TIME_POS, '%B')
t_precip <- tapply(df.tmp$PRECIP_in, list(df.tmp$DATE_TIME_POS$year + 1900, df.tmp$month), sum)

t_precip[is.na(t_precip)] <- 0
is.na(t_precip)

barplot(t_precip, beside=F, col=c('gray45', 'yellow', '#D4FBB0'), xlab='Month of year', ylab='Total precipitation(inch)'
        , main = 'Prepication by month and year')

legend(x='topright', legend=c('2016', '2017', '2018'), fill=c('gray45', 'yellow', '#D4FBB0'), cex=0.8)

