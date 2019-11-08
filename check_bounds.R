check_bounds <- function(lower_bound, upper_bound, number) {
  
  if (number > lower_bound & number < upper_bound) return(TRUE)
  return(FALSE)
}