create_population <- function(size, upper_bound, lower_bound) {
  
  x <- runif(size, lower_bound, upper_bound)
  y <- runif(size, lower_bound, upper_bound)
  fitness <- rep(0, size)
  
  return(data.frame(x, y, fitness))
}