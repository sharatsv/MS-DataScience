# LiveClass-4

v <- rnorm(1000,50,10) #(obs-100, mean-50 & sd-10)
hist(v)
# quantile function on mid-term quiz!
q <- quantile(v, c(.1,.9))
q

q <- quantile(v, c(.1,.5,.9))
q

# NOTE: q is essentially a vector
labels(q)
q[1]
q[2]
q[3]

# skewness function
library("moments")
skewness(v)

# replicate function
pb <- "peanut butter"
vc <- ""
