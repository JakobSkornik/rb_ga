mutation <- function(parents, parental_presence, ap) {
  
  offspring <- parents[FALSE,]
  
  for (i in 1:(parental_presence / 2)) {
      
    a <- parents[i,]$x
    b <- parents[i,]$y
    
    x <- a*runif(1, a - ap/2, a + ap/2)
    y <- b*runif(1, b - ap/2, b + ap/2)
    fitness <- 0
    kid <- data.frame(x, y, fitness)
    offspring <- rbind(offspring, kid)
  }
  
  return(offspring)
}
