# Exploring data-frames
myfamilynames <- c('dad','mom','sis','bro','dog')
myfamilyages <- c(43,42,12,8,5)
myfamilygender <- c('m','f','f','m','f')
myfamilyweights <- c(188,136,83,61,44)

# Create a data-frame
df <- data.frame(myfamilynames,myfamilyages,myfamilygender,myfamilyweights)
df

# Add a new row/data-entry into the data-frame
# example: df <- rbind(df,<vector>)
df_new <- data.frame(myfamilynames,myfamilyages,myfamilygender,
                     myfamilyweights,stringsAsFactors = FALSE) #stringAsFactors - FALSE
str(df_new) #see myfamilynames & myfamilygender as 'chr'
df_new <- rbind(df_new, c('cat',3,'f',22))
df_new

# Add a new col/attribute into the data-frame
# example: df <- cbind(df, <vector>)
myfamilyheights <-c(72,60,50,40,12,8)
df_new <- cbind(df_new, myfamilyheights)
df_new

# Vector math withing a data-frame (add 2-years to myfamilyages)
df$myfamilyages <- df$myfamilyages + 2
df

# To do the same thing on df_new convert the myfamilyages back to num
df_new$myfamilyages <- as.numeric(df_new$myfamilyages)
str(df_new)
df_new$myfamilyages <- df_new$myfamilyages + 2
df_new

# Add a new-column without using cbind
df_new$myfamilyweights <- as.numeric(df_new$myfamilyweights)
df_new$weightheightratio <- df_new$myfamilyweights/df_new$myfamilyheights
df_new

# To get all ages > 10
highages <- df_new[df_new$myfamilyages > 10,]
highages

# Sort based on a column
df_new_sorted <- df_new[order(df_new$weightheightratio),]
df_new_sorted
# Sample-output:
# > df_new_sorted
# myfamilynames myfamilyages myfamilygender myfamilyweights myfamilyheights weightheightratio
# 4           bro           10              m              61              40          1.525000
# 3           sis           14              f              83              50          1.660000
# 2           mom           44              f             136              60          2.266667
# 1           dad           45              m             188              72          2.611111
# 6           cat            5              f              22               8          2.750000
# 5           dog            7              f              44              12          3.666667

# 