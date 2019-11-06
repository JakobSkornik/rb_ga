addResult <- function(r, g, v, f, m) {
  
  Generation <- g
  Fittest <- f
  Solution <- v
  Mean <- m
  df <- data.frame(Generation, Fittest, Solution, Mean)
  return(rbind(r, df))
}