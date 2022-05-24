# Week-3 Homework
# Describe a data-set

fname <- file.choose()
data <- read.csv(fname, sep=',', header=T, stringsAsFactors=F)
str(data)
head(data)
colnames(data)
dim(data)

# Shorten hostnames (remove part of the FQDN)
library(stringr)
data$hostname <- str_replace(data$hostname, '.nsbucqesystem.test', '')


# Visualizing a network topology dynamically using logs emitted and aggregated.

# Plot-1: How hot is the network (eventually a heat-map)? 

# Plot-2:
# Bar-plot of severity of errors - that is, severity level and number of logs in the time-period
sev <- table(data$severity)

# From colorcodehex.com:
#   - blue - #2C85EF
#   - green - #089E41
barplot(log(sev), ylab = 'Severity-Type', xlab = 'Count (log-scale)', main = 'Alerts by severity-type in network'
        , col = c('red', '#2C85EF', '#089E41', 'orange') 
        , horiz= T)

# Plot-3:
# Bar-plot of all errors grouped by nodes
data$hostname
nodes_df <- data.frame(table(data$hostname))
colnames(nodes_df) <- c('Node', 'Error_Count')
nodes_df_sorted <- nodes_df[order(nodes_df$Error_Count, decreasing = F), ]

# Adjust margins
par(mar = c(2,10,5,2))

# Red - #CE2F43
barplot(nodes_df_sorted$Error_Count, names.arg = nodes_df_sorted$Node
       , xlab = 'All errors', main = 'Alerts by node-type in network'
       , col = '#CE2F43'
       , horiz= T, las=1)


# Plot-4: Scatter plot of errors by time colored as follows:
# NOTICE - green
# WARNING - orange
# INFO - orange
# ERROR - red
col.vec <- rep('orange', nrow(data))
col.vec[data$severity == 'notice'] <- 'green'
col.vec[data$severity == 'err'] <- 'red'

# Encode values NOTICE, WARNING, INFO and ERROR in data
encode_ordinal <- function(x, order = c('notice', 'info', 'warning', 'err')) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

data$severity_enc <- encode_ordinal(data$severity)

# Convert the data$time to a time format using strptime
data$time <- strptime(data$time, format = "%Y-%m-%d %H:%M:%OS")

# Make a point/scatter plot
par(mfrow=c(2,2))

data_1 <- data[data$severity_enc == 1,]
data_2 <- data[data$severity_enc == 2,]
data_3 <- data[data$severity_enc == 3,]
data_4 <- data[data$severity_enc == 4,]

plot(data_1$time, data_1$severity_enc
     , xlab = 'Time'
     , ylab = 'Severity'
     , main = 'Errors by sev(NOTICE) across time-of-day'
     , pch = 16
     , cex = 0.8
     , col = '#089E41')

plot(data_2$time, data_2$severity_enc
     , xlab = 'Time'
     , ylab = 'Severity'
     , main = 'Errors by sev(INFO) across time-of-day'
     , pch = 16
     , cex = 0.8
     , col = '#2C85EF')

plot(data_3$time, data_3$severity_enc
     , xlab = 'Time'
     , ylab = 'Severity'
     , main = 'Errors by sev(WARN) across time-of-day'
     , pch=16
     , cex=0.8
     , col='orange')

plot(data_4$time, data_4$severity_enc
     , xlab = 'Time'
     , ylab = 'Severity'
     , main = 'Errors by sev(ERR) across time-of-day'
     , pch=16
     , cex=0.8
     , col='red')

# Plot-4: Severity grouped by components
sev_xplane <- table(data$severity[grepl("prom|nsx-mgr", data$hostname)], data$hostname[grepl("prom|nsx-mgr", data$hostname)])
sev_edges <- table(data$severity[grepl("edge", data$hostname)], data$hostname[grepl("edge", data$hostname)])
sev_rem_nodes <-  table(data$severity[grepl("20.20.", data$hostname)], data$hostname[grepl("20.20.", data$hostname)])
class(sev_xplane)
par(mfrow = c(2,2), mar = c(5,9,3,3.8))
barplot(sev_xplane, xlab = 'Count', main = 'Alerts - Control/Data Plane of network'
        , col = c('red', '#2C85EF', '#089E41', 'orange') 
        , horiz = T
        , las = 1)


par(mar = c(8,9,4,3))
barplot(sev_edges, xlab = 'Count', main = 'Alerts - Edge-nodes of network'
        , col = c('red', '#2C85EF', '#089E41', 'orange')
        , horiz = T
        , las = 1)

par(mar = c(7,9,4,2))
barplot(sev_rem_nodes, xlab = 'Count', main = 'Alerts - Rest of network'
        , col = '#2C85EF'
        , horiz = T
        , las = 1)

barplot(rep(NA, 5), ylim = c(0,5), main = 'Legend'
        , xaxt='n', yaxt='n'
        , axes = F)

legend(x = 'center'
       , legend=c('err', 'info', 'notice', 'warning')
       , fill = c('red', '#2C85EF', '#089E41', 'orange')
       , border = 'black'
       , title = 'Severity')

# Finally, let's visualize via a pie-chart severity distribution for each node-type
# Walk all the columns of the table sev_xplane
par(mfrow = c(2,2), mar = c(2,9,3,3.9))
pie(apply(sev_xplane, 1, sum), main = 'Health - Control/Data Plane of network'
    , col = c('red', '#2C85EF', '#089E41', 'orange') 
    , labels = NA)

par(mar = c(2,9,3,1))
pie(apply(sev_edges, 1, sum), main = 'Health - Edge-nodes of network'
    , col = c('red', 'orange')
    , labels = NA)

par(mar = c(2,9,3,1))
pie(apply(sev_rem_nodes, 1, sum), main = 'Health - Rest of network'
    , col = '#2C85EF'
    , labels = NA)


barplot(rep(NA, 5), ylim = c(0,5), main = 'Legend'
        , xaxt='n', yaxt='n'
        , axes = F)

legend(x = 'top'
       , legend=c('err', 'info', 'notice', 'warning')
       , fill = c('red', '#2C85EF', '#089E41', 'orange')
       , border = 'black'
       , title = 'Severity')



