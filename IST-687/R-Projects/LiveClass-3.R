#Breakout exercise
brkout2 <- function(a, pos1, pos2){
  min_val <- min(a)
  max_val <- max(a)
  if(pos1 < min_val | pos1 > max_val) {
    print("Invalid pos1 arg!")
    return(pos1)}
  if(pos2 < min_val | pos2 > max_val) { 
    print("Invalid pos2 arg!")
    return(pos2) }
  cat(pos1,"position value is",a[pos1])
  cat("\n")
  cat(pos2,"position value is",a[pos2])
}

# Define v
v <- sample(1:20)
# See v since it's random sample/order of 20 values
v
# Call function brkout2
brkout2(v, 0, 20)
