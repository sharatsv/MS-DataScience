#
#      Course: IST-687
#      Name: Sharat Sripada
#      Homework #2
#      Due Date: 1/26/2020
#      Date Submitted: 1/26/2020
#

data()
myCars <- mtcars

# ---- Step1: What is the hp -------------
# Get index of max hp/horse-power
MaxhpIndex <- which.max(myCars$hp)

# Get the max-hp value
Maxhpval <- myCars[MaxhpIndex, 4]

# Verify if there are >1 cars with hp equals Maxhpval
if (length(row.names(myCars[myCars$hp == Maxhpval,])) > 1) ">1 cars" else "=1 cars"

# Get the car-name with max-hp 
Maxhpcar <- row.names(myCars[MaxhpIndex,])

# Print the car-name with max-hp & the corresponding hp
cat(Maxhpcar, "has max hp", Maxhpval)

# ---- Step2: Explore mpg -------------
# Get index of max mpg/mile per gallon
MaxmpgIndex <- which.max(myCars$mpg)

# Get the max-mpg value
Maxmpgval <- myCars[MaxmpgIndex, 1]

# Verify if there are >1 cars with hp equals Maxmpgval
if (length(row.names(myCars[myCars$mpg == Maxmpgval,])) > 1) ">1 cars" else "=1 cars"

# Get the car-name with max-mpg 
Maxmpgcar <- row.names(myCars[MaxmpgIndex,])

# Print the car-name with max-mpg & the corresponding hp
cat(Maxmpgcar, "has max mpg", Maxmpgval)

# Sort based on mpg
myCars[order(myCars$mpg),]

# ---- Step3: Which	car	has	the	“best” combination of	mpg	and	hp? -------------

# Logic-1: Use a combination of (mpg, hp) to order the data -> pick the middle value
# Order the data
newdata <- myCars[order(myCars$mpg, myCars$hp),]

# Select the middle value and print the car-name
newdata[length(row.names(myCars))/2,]
row.names(newdata[length(row.names(myCars))/2,])

# Logic-2: Calculate a ratio of mpg/hp & create a new column -> order it -> pick the midlle value

# Create a new column with ratio mpg/hp
myCars$MpgHpRatio <- myCars$mpg/myCars$hp

# Order the data
newdata <- myCars[order(myCars$MpgHpRatio), ]

# Select the middle value and print the car-name
newdata[length(row.names(myCars))/2,]
row.names(newdata[length(row.names(myCars))/2,])

# ---- Step4: Which	car	has “best”	car combination	of	mpg	and	hp,	where	mpg	and	hp	must be	given	equal	weight? ---
# Using scale in its default form to normalize the mpg and hp columns
# Scale funtionality:
#   Z-score = (x - u) / SD
myCars$mpgscale <- scale(myCars$mpg)
myCars$hpscale <- scale(myCars$hp)

# Order based on column mpgscale, hpscale
newdata <- myCars[order(myCars$mpgscale, myCars$hpscale),]

# Select the middle value and print the car-name
newdata[length(row.names(myCars))/2,]
row.names(newdata[length(row.names(myCars))/2,])






