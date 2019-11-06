source('rosenbrock.R')

x <- seq(from = -1, to =  1, by = 0.1)
y <- x
z <- matrix(0, 20, 20)

persp(x, y, z = outer(x, y, rosenbrock),
      xlab = 'x',
      ylab = 'y',
      zlab = 'z',
      theta = 95,
      main = 'Rosenbrock')
