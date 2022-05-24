# Live-Session 6
install.packages("ggplot2")
library(ggplot2)

ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=2,color='black',fill='pink') + ggtitle("Example: mtcars, mpg")

# Explore more from class here - See class notes