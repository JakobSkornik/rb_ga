genalg <- function(pop_size, stop, parental_presence, generation_survivability, alpha, lower_bound, upper_bound, extended) {
  
  #source functions
  source('addResult.R')
  source('input.R')
  source('create_population.R')
  source('calculate_fitnes.R')
  source('selection.R')
  source('visualize.R')
  
  #select preferred mutation type
  #source('crossover.R')
  #source('crossover_2.R')
  source('mutation.R')
  
  conditions <- input(pop_size, stop, parental_presence, generation_survivability, alpha, lower_bound, upper_bound)
  
  if (all(conditions != 'OK'))
    stop(conditions)
  
  result <- data.frame( Generation=numeric(),
                        Fittest=numeric(),
                        x=numeric(),
                        y=numeric(),
                        Mean=numeric())
  
  result_extensive <- data.frame(
                        Generation=numeric(),
                        Fitness=numeric(),
                        x=numeric(),
                        y=numeric(),
                        Mean=numeric())
  
  Generation  <- 0
  Fittest     <- 0
  Fitness     <- 0
  x           <- 0
  y           <- 0
  Mean        <- 0
  result      <- addResult(result, Generation, Fittest, x, y, Mean)
  
  result_extensive <- data.frame(
                        Generation=numeric(),
                        Fitness=numeric(),
                        x=numeric(),
                        y=numeric(),
                        Mean=numeric())
  
  #create random initial population and calculate fitness of individuals
  population <- create_population(pop_size, upper_bound, lower_bound)
  population <- calculate_fitnes(population, pop_size)
  
  #setting some vars
  generation      <- 1
  number_of_gens  <- stop  
  previous        <- population[1,3]
  
  result          <- addResult(result, 
                               generation, 
                               population[1,3], 
                               population[1,1], 
                               population[1,2], 
                               mean(population[,3]))
  
  result_extensive<- addResult(result_extensive, 
                               generation, 
                               population[,3], 
                               population[,1], 
                               population[,2], 
                               mean(population[,3]))

  #main while loop
  while (stop > 0) {
    
    #select parents
    parents     <- selection(population, pop_size, parental_presence)
    
    #produce offspring
    offspring   <- mutation(parents, parental_presence, alpha)
    population  <- rbind (population[1:generation_survivability, ], 
                          offspring, 
                          create_population(pop_size - generation_survivability - parental_presence/2, 
                                            upper_bound, 
                                            lower_bound))
    
    #calculate new populations fitness
    population  <- calculate_fitnes(population, pop_size)
    
    generation  <- generation + 1
        
    result      <- addResult(result, 
                             generation, 
                             population[1,3], 
                             population[1,1], 
                             population[1,2], 
                             mean(population[,3]))
    
    result_extensive<- addResult(result_extensive, 
                             generation, 
                             population[,3], 
                             population[,1], 
                             population[,2], 
                             mean(population[,3]))
    
    #if the result doesnt change for 'stop' number of generations, the while loop stops
    if (isTRUE(all.equal(population[1,3], previous, tolerance = 0.00001)))
      stop <- stop - 1
    
    else {
      
      stop      <- number_of_gens
      previous  <- population[1,3]
    }
  }
  
  
  
  if (extended == 1)
    return(result_extensive)
  
  return(result)
}
