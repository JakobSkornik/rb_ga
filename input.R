input <- function (pop_size, stop, parental_presence, generation_survivability, alpha, lower_bound, upper_bound, crossover) {
  
  if (parental_presence > pop_size)
    return('Population size smaller than parental presence!')
  
  if (generation_survivability > pop_size - parental_presence / 2)
    return('Generation survivability bigger future population sizes! (Too many offspring, or survivability higher than population size.)')
  
  if (lower_bound > upper_bound)
    return('Lower bound is higher than upper bound!')
  
  if (alpha < 0)
    return('Alpha not in range (0,1)')
  
  if (alpha > 1)
    return('Alpha not in range (0,1)')
  
  return('OK')
}