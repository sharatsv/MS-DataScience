#
#      Course: IST-687
#      Name: Sharat Sripada
#      Homework #4
#      Due Date: 2/23/2020
#      Date Submitted: 2/23/2020
#      Topic: Using ggplot to visualize data (Air Quality Analysis)

# install.packages("ggplot2")
# install.packages("reshape2")

library("ggplot2")
library("reshape2")

air <- airquality

# Function to plot a histogram given a data-frame, column-name
my_ggplot_hist <- function(my_df, my_col){
  g <- ggplot(my_df, aes(x=my_col))
  g <- g + geom_histogram(binwidth = 5, color = 'white', fill = 'black')
  g
  
}

# Function to plot a box-plot given a data-frame, column-name
my_ggplot_box <- function(my_df, my_col){
  g <- ggplot(my_df, aes(x = ' ', y = my_col))
  g <- g + geom_boxplot()
  g
}

# Function to plot a line plot given a data-frame, x & y axes
my_ggplot_line <- function(my_df, my_x, my_y){
  g <- ggplot(my_df, aes(x = my_x, y = my_y, group = 1))
  g <- g + geom_line()
  g
}


# Step-2: Clean the Data
# Calculate mean for the Ozone column
ozone_mean <- mean(air$Ozone, na.rm = TRUE)
cat("Replacing NAs in Ozone column with mean =", ozone_mean)
air$Ozone[is.na(air$Ozone)] <- ozone_mean

# Calculate mean for the Solar.R column 
solar_mean <- mean(air$Solar.R, na.rm = TRUE)
cat("Replacing NAs in Solar column with mean =", solar_mean)
air$Solar.R[is.na(air$Solar.R)] <- solar_mean

# Print the columns & examine for NAs
print(air$Ozone)
print(air$Solar.R)

# Step-3: 
# Create a histogram for the Ozone column as X var.
my_ggplot_hist(air, air$Ozone)

# Create a histogram for the Solar column as X var.
my_ggplot_hist(air, air$Solar.R)

# Create a histogram for the Wind column as X var.
my_ggplot_hist(air, air$Wind)

# Create a histogram for the Temp column as X var.
my_ggplot_hist(air, air$Temp)

# Create a histogram for the Month column as X var.
my_ggplot_hist(air, air$Month)

# Create a histogram for the Day column as X var.
my_ggplot_hist(air, air$Day)

# Create a box-plot for Ozone
my_ggplot_box(air, air$Ozone)

# Create a box-plot for Wind
my_ggplot_box(air, air$Wind)
# Reading the box-plot visually:
# min = 1.25
# max = 20.625
# Q1 = 7.5
# Q3 = 11.25
# Median = 9.375
summary(air$Wind)
# TO-DO: Work on reading box-plots more accurately - Can you change Axis to granular values?

# Step-3:
# Create a new column in df air called Date
air$Date <- gsub(" ", "", paste("1973", "-", air$Month, "-", air$Day))
print(air$Date)

# Create a line plot for Ozone
my_ggplot_line(air, air$Date, air$Ozone)

# Create a line plot for Temp
my_ggplot_line(air, air$Date, air$Temp)

# Create a line plot for Wind
my_ggplot_line(air, air$Date, air$Wind)

# Create a line plot for Solar.R
my_ggplot_line(air, air$Date, air$Solar.R)

# Create one chart comprising all 4x lines in different colors
g <- ggplot(air, aes(x = Date, group=1))
g <- g + geom_line(aes(y = Ozone, group=1), color = "red")
g <- g + geom_line(aes(y = Temp, group=1), color = "orange")
g <- g + geom_line(aes(y = Wind, group=1), color = "black")
g <- g + geom_line(aes(y = Solar.R, group=1), color = "blue")
g

# Scale the Wind data/column - Create a new column WindScaled in air df
air$WindScaled <- air$Wind * 10
g <- ggplot(air, aes(x = Date, group=1))
g <- g + geom_line(aes(y = Ozone, group=1), color = "red")
g <- g + geom_line(aes(y = Temp, group=1), color = "orange")
g <- g + geom_line(aes(y = WindScaled, group=1), color = "black")
g <- g + geom_line(aes(y = Solar.R, group=1), color = "blue")
g

# Create a new data-frame picking Date, Ozone, Temp, WindScaled, Solar.R from air
new_air <- data.frame(air$Date, air$Ozone, air$Solar.R, air$Temp, air$WindScaled)

# Using the melt function from reshape2 shrink/collapse table
melt_new_air <- melt(new_air, id.vars=1)
g <- ggplot(melt_new_air, aes(x = air.Date, y = value, col = variable, group = 1))
g <- g + geom_line()
g

# Step-4: Create a heap-map using the geom_tile function
g <- ggplot(melt_new_air, aes(air.Date, variable))
g <- g + geom_tile(aes(fill = value) , color = "white" ) + scale_fill_gradient(low = "white", high = "steelblue")
g

# Step-5: Create a scatter plot
g <- ggplot(air, aes(x=Wind, y=Temp))
g <- g + geom_point(aes(size=Ozone, color=Solar.R))
g


