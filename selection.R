source('normalize.R')

selection <- function(population, size, parental_persistence) {
  
  working_population <- population  
  parents <- population[FALSE,]
  temp <- working_population
  
  temp$fitness <- normalize(sum(working_population$fitness, na.rm = TRUE), temp$fitness)
  temp[, 3] <- cumsum(temp[, 3])
  
  for (i in 1:parental_persistence) {
    
    lottery <- runif(1)
    selected <- min(which(temp[, 3] > lottery))
    parents <- rbind(parents, working_population[selected, ])
    
    working_population <- working_population[-selected, ]
    temp <- working_population
    temp$fitness <- normalize(sum(working_population$fitness, na.rm = TRUE), temp$fitness)
    temp[, 3] <- cumsum(temp[, 3])
  }
  
  return(parents)
}
