genalg <- function(pop_size, stop, parental_persistence, alpha, lower_bound, upper_bound) {
  
  result <- data.frame( Generation=numeric(),
                        Fittest=numeric(),
                        Solution=vector(),
                        Mean=numeric())
  
  #source functions
  source('addResult.R')
  source('create_population.R')
  source('calculate_fitnes.R')
  source('selection.R')
  source('mutation.R')
  
  #create random initial population and calculate fitness of individuals
  population <- create_population(pop_size, upper_bound, lower_bound)
  population <- calculate_fitnes(population, pop_size)
  
  #setting some vars
  generation <- 0
  number_of_gens <- stop  
  previous <- population[1,3]
  
  result <- addResult(result, generation, population[1,3], c(population[1,1], population[1,2]), mean(population[,3]))

  #main while loop
  while (stop > 0) {
    
    #select parents
    parents <- selection(population, pop_size, parental_persistence)
    
    #produce offspring
    offspring <- mutation(parents, parental_persistence, alpha)
    population <- rbind (population[1:3, ], offspring, create_population(pop_size - 3 - parental_persistence/2, upper_bound, lower_bound))
    
    #calculate new populations fitness
    population <- calculate_fitnes(population, pop_size)
    
    generation <- generation + 1
    
    result <- addResult(result, generation, population[1,3], c(population[1,1], population[1,2]), mean(population[,3]))
    
    #if the result doesnt change for 'stop' number of generations, the while loop stops
    if (isTRUE(all.equal(population[1,3], previous, tolerance = 0.001)))
      stop <- stop - 1
    
    else {
      
      stop <- number_of_gens
      previous <- population[1,3]
    }
  }
  
  return(result)
}
