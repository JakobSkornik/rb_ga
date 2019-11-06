source('rosenbrock.R')

x <- seq(from = -1, to =  1, by = 0.1)
y <- x
z <- outer(x, y, rosenbrock)

persp(x, y, z,
      xlab = 'x',
      ylab = 'y',
      zlab = 'z',
      theta = 95,
      main = 'Rosenbrock')
