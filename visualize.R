visualize <- function(df, lower_bound, upper_bound) {
  
  source('rosenbrock.R')
  #I am using rgl package as a tool to visualize
  library(rgl)
  
  plot(df$Generation, df$Fittest, type = 'l')
  lines(df$Generation, df$Mean, col = 'green')
  
  rgl.open()
  persp3d(rosenbrock, 
          xlim = c(lower_bound, upper_bound), 
          ylim = c(lower_bound, upper_bound),
          col = 'white',
          theta = 270)
  
  rgl.spheres(df$x, df$y, df[, 2], r = upper_bound / 20, color = "red")
}

