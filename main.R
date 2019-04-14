rm(list = ls())
library(mvtnorm)
library(RUnit)
source("helpers/helpers.R")
source("helpers/1_density.R")
source("helpers/2_particle.R")
source("helpers/3_universe.R")


new("Particle")
m = list(c(1,2), c(3,4), c(5,6))
sigma = list(diag(2), diag(2), diag(2))
type = list(c(-1,1), c(1,1), c(1,-1))
bounds = c(+Inf, +Inf)
sum_elem = 0L
u = new("Universe", m, sigma, type, bounds, sum_elem)

u@f(c(1,2),diag(2))(c(0,0))
u@Df(c(1,2),diag(2))(c(0,0))

get_m(u)
mix(u)(c(0,0))
# methods: proportion of type=+1 etc.

# evolution is t particules position at each step
# particle is the complete particles at the current step
# dim = ...     type can be (+,+,-) in dim 3!!!! Yes









## Manifold parameters
bounds = c(2*pi, NA)
sum_elem = 10L

## Particle parameters
m = list(c(0,0)) # runif(n, -length_circle/2, length_circle/2) #c(-1, 0, 1)
sigma = diag(2)
type = +1 # 2*(rbinom(n, 1, 0.5) - 0.5) # c(+1, +1, -1) # rep(-1, n)

## Evolution parameters
alpha = 0.1


push(m, type, alpha, Df, sigma, bounds, sum_elem)
push(0.2, type, alpha, Df, sigma, bounds, sum_elem)



########
# Code #
########
nsteps = 10
every = 1
x = seq(from = -length_circle/2, to = length_circle/2, length.out = 1000)
m_all = matrix(NA, ncol = length(m), nrow = nsteps)
m_all[1, ] = m
for(step in 2:nsteps) {
  m_all[step, ] = push(m_all[step-1, ], type, sigma, alpha, Df)
  
  if(step %% every == 0) {
    dir.create("plot", showWarnings = FALSE)
    png(paste0("plot/", step, ".png"))
    plotting(x, m_all[step, ], type, sigma(step), f, Df, with_deriv = FALSE, main = step)
    dev.off()
  }
}

# plot(m_all[,1], type = "l", ylim = range(m_all))
# lines(m_all[,2], type = "l", col = "blue")
sum(type == 1)
