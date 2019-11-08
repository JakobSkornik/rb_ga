mutation <- function(parents, parental_presence, ap, lower_bound, upper_bound) {
  
  offspring <- parents[FALSE,]
  
  for (i in 1:(parental_presence)) {
      
    a <- parents[i,]$x
    b <- parents[i,]$y
    
    x <- runif(1, (a - ap/2), (a + ap/2))
    y <- runif(1, (b - ap/2), (b + ap/2))
    fitness <- 0
    
    if(check_bounds(lower_bound, upper_bound, x) & check_bounds(lower_bound, upper_bound, y)){
    
      kid <- data.frame(x, y, fitness)
      offspring <- rbind(offspring, kid)
    }
  }
  
  return(offspring)
}
