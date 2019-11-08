mutation <- function(parents, parental_presence, ap, lower_bound, upper_bound) {
  
  offspring <- parents[FALSE,]
  
  for (i in 1:parental_presence) {
    
    if (i %% 2 == 0) {
      
      R <- sample(1:2, 1)
      x <- parents[i + 1 - R,]$x
      y <- parents[i + 1 - R,]$y
      fitness <- 0
      kid <- data.frame(x, y, fitness)
      offspring <- rbind(offspring, kid)
    }
  }
  
  return(offspring)
}