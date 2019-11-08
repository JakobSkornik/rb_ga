mutation <- function(parents, parental_presence, ap, lower_bound, upper_bound) {
  
  offspring <- parents[FALSE,]
  
  for (i in 1:parental_presence) {
    
    if (i %% 2 == 0) {
      
      a <- parents[i - 1,]$x
      b <- parents[i - 1,]$y
      c <- parents[i,]$x
      d <- parents[i,]$y
      
      x <- a*ap + (1 - ap) * c
      y <- b*ap + (1 - ap) * d
      fitness <- 0
      
      if(check_bounds(lower_bound, upper_bound, x) & check_bounds(lower_bound, upper_bound, y)){
        
        kid <- data.frame(x, y, fitness)
        offspring <- rbind(offspring, kid)
      }
    }
  }
  
  return(offspring)
}