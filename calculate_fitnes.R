source('fitnes.R')

calculate_fitnes <- function(population, size) { 
  
  population$fitness <- fitnes(population$x, population$y)
  
  return(population[order(-population$fitness),])
}