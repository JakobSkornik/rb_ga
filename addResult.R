addResult <- function(r, g, f, x, y, m) {
  
  Generation <- g
  Fittest <- f
  Mean <- m
  df <- data.frame(Generation, Fittest, x, y, Mean)
  return(rbind(r, df))
}