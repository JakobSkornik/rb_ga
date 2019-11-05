create_population <- function(size, upper_bound, lower_bound) {
  
  x <- runif(size, lower_bound, upper_bound)
  y <- runif(size, lower_bound, upper_bound)
  fitness <- rep(0, size)
  
  population <- data.frame(x, y, fitness)
  return(population)
}

fitnes <- function(x, y) {  return(rosenbrock(x, y))}

calculate_fitnes <- function(population, size) { 
  
  population$fitness <- fitnes(population$x, population$y)
  population <- population[order(-population$fitness),]
  return(population)}

selection <- function(population, size, parental_persistence) {
  
  initial_population <- population  
  parents <- population[FALSE,]
  temp <- initial_population
  
  temp$fitness <- normalize(sum(initial_population$fitness, na.rm = TRUE), temp$fitness)
  temp[, 3] <- cumsum(temp[, 3])

  for (i in 1:parental_persistence) {
    
    who <- runif(1)
    selected <- min(which(temp[, 3] > who))
    parents <- rbind(parents, initial_population[selected, ])
    
    initial_population <- initial_population[-selected, ]
    temp <- initial_population
    temp$fitness <- normalize(sum(initial_population$fitness, na.rm = TRUE), temp$fitness)
    temp[, 3] <- cumsum(temp[, 3])
  }
  
  return(parents)
  #print(parents)
} 

normalize <- function(div, number) { return(number / div) }

mutation <- function(parents, parental_persistence, ap) {
  
  offspring <- parents[FALSE,]
  
  for (i in 1:parental_persistence) {
    
    if (i %% 2 == 0) {
      
      a <- parents[i - 1,]$x
      b <- parents[i - 1,]$y
      c <- parents[i,]$x
      d <- parents[i,]$y
      
      x <- a*ap + (1 - ap) * c
      y <- b*ap + (1 - ap) * d
      fitness <- 0
      kid <- data.frame(x, y, fitness)
      offspring <- rbind(offspring, kid)
    }
  }
  
  return(offspring)
}

genalg <- function(pop_size, stop, parental_persistence, alpha, lower_bound, upper_bound) {
  
  number_of_gens <- stop
  
  source('rosenbrock.R')
  source('genalg.R')
  
  population <- create_population(pop_size, upper_bound, lower_bound)
  
  population <- calculate_fitnes(population, pop_size)
  
  generation <- 1
  
  h <- vector()
  
  previous <- population[1,3]

  while (stop > 0) {
    
    print('generation')
    print(generation)
    print(population)
    generation <- generation + 1
    
    parents <- selection(population, pop_size, parental_persistence)
    
    offspring <- mutation(parents, parental_persistence, alpha)

    population <- rbind (population[1:3, ], offspring, create_population(pop_size - 3 - parental_persistence/2, upper_bound, lower_bound))
    
    population <- calculate_fitnes(population, pop_size)
    
    h <- c(h, population[1,3])
    
    if (isTRUE(all.equal(population[1,3], previous, tolerance = 0.0001)))
      stop <- stop - 1
    
    else {
      
      stop <- number_of_gens
      previous <- population[1,3]
    }
  }
  
  plot(h, type="l", xlab = 'Generation', ylab = 'Maximum')
}
