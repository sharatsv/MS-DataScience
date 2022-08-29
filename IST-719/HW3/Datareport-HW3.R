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



# Make a spatial graph of the network
# Data shape should look like the following:
# +--------------------------------------------------------+
# | id | Node  | Info | Error | Warning | Notice |  Health |
# +--------------------------------------------------------+
#
# where:
#   - health = 0.1 * Info/Total + 0.2 * Warning/Total + 0.3 * Notice/Total + 0.4 * Error/Total
# 
df1 <- as.data.frame.matrix(t(sev_xplane))
df2 <- as.data.frame.matrix(t(sev_edges))
# Add missing columns
df2$notice <- 0
df2$warning <- 0

df3 <- as.data.frame.matrix(t(sev_rem_nodes))
# Add missing columns
df3$err <- 0
df3$notice <- 0
df3$warning <- 0
# Exercise: Re-order columns if needed (rbind takes care of it anyways)
# df3 <- df3[, c(2,1,3,4)]

nodes <- rbind(df1, df2, df3)

# Normalize and calculate a health we can use later
nodes$health <- as.integer(10*(nodes$info/(nodes$info + nodes$err + nodes$notice + nodes$warning)) + 
  20*(nodes$warning/(nodes$info + nodes$err + nodes$notice + nodes$warning)) + 
  30*(nodes$notice/(nodes$info + nodes$err + nodes$notice + nodes$warning)) + 
  40*(nodes$err/(nodes$info + nodes$err + nodes$notice + nodes$warning)))

# Set mainly three colors for health score
nodes$health_colr[nodes$health > 30] <- 'red'
nodes$health_colr[nodes$health <= 30] <- 'orange'
nodes$health_colr[nodes$health <= 10] <- 'green'


# Convert rowname to column using tibble
library(tibble)
nodes <- tibble::rownames_to_column(nodes, 'id')

# Form a new data-frame with links - comprising id, links, weights etc.
from <- c('nsx-mgr-0', 'nsx-mgr-0', 'nsx-mgr-0', 'nsx-mgr-0', 'nsx-mgr-0', 'nsx-mgr-0', 'nsx-mgr-0', 
          'nsx-mgr-0', '20.20.216.143', '20.20.216.150', '20.20.216.151', '20.20.216.152', '20.20.216.153',
          '20.20.216.154', '20.20.216.155')
to <- c('prom-05056823bd7', 'prom-0505682571', 'prom-0505682662c', 'prom-0505682c032', 
        'prom-0505682c3b6', 'prom-0505682edf8', 'nsx-edge-0-c0', 'nsx-edge-1-c0', 'prom-05056823bd7', 'prom-0505682571',
        'prom-0505682662c', 'prom-0505682c032', 'prom-0505682c3b6', 'prom-0505682edf8', 'prom-05056823bd7')
length(from)
length(to)
type <- rep('mention', length(from))
links <- data.frame(from, to, type)

# Using igraph let's make a network map
library(igraph)
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F)


# Get edge properties - importantly set width of each edge node based 
# on weight of the node
V(net)$size <- V(net)$health*0.4
V(net)$frame.color <- 'white'
V(net)$color <- V(net)$health_colr

plot(net,edge.arrow.size=.4,col='green', vertex.label.color="black"
     ,vertex.label.dist=1, vertex.label.font=1)


