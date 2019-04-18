####################
# Mixture function #
####################
mix_func = function(f, particles, bound = +Inf, sum_elem = 3L) {
  dim = length(particles[[1]])
  bounds = rep(bound, dim)
  function(x) {
    out = sapply(particles, function(particle_pos) {
      f(particle_pos, bounds, sum_elem)(x)
    })
    if(is.null(dim(out)[1])) { # 1D = Evaluation of a density
      return(mean(out))
    } else { # 2D or more = Evaluation of the derivative
      return(apply(out, 1, mean))
    }
  }
}

## Examples
# particles = list(c(0,0,0), c(0,0,1), c(1,0,0), c(2,0,1))
# bound = +Inf
# sum_elem = 3L
# mix_func(f, particles, bound, sum_elem)(c(0,0,0))
# mix_func(f, particles, bound, sum_elem)(c(0,0,1))
# mix_func(f, particles, bound, sum_elem)(c(1,0,0))
# mix_func(f, particles, bound, sum_elem)(c(2,0,1))
# mix_func(Df, particles, bound, sum_elem)(c(0,0,0))
# mix_func(Df, particles, bound, sum_elem)(c(0,0,1))
# mix_func(Df, particles, bound, sum_elem)(c(1,0,0))
# mix_func(Df, particles, bound, sum_elem)(c(2,0,1))
# 
# particles = list(c(0,0,0), c(0,0,1))
# bound = +Inf
# sum_elem = 3L
# mix_func(f, particles, bound, sum_elem)(c(0,0,0))
# mix_func(f, particles, bound, sum_elem)(c(0,0,1))
# mix_func(Df, particles, bound, sum_elem)(c(0,0,0))
# mix_func(Df, particles, bound, sum_elem)(c(0,0,1))

####################
# Pushing one step #
####################
push = function(particles, types, alpha = 0.1, Df, bound = +Inf, sum_elem = 3L) {
  Dg = mix_func(Df, particles, bound, sum_elem)
  matrix_mix_applied_in_each_part = sapply(particles, Dg) # matrix of shape (dim, nb_particles) 
  matrix_types = sapply(types, c)
  
  out = sapply(particles, c) + matrix_types * alpha * matrix_mix_applied_in_each_part
  out = lapply(1:ncol(out), function(col){out[, col]})
  return(out)
}

## Example
# particles = list(c(0,0,0), c(0,0,1), c(1,0,0), c(2,0,1))
# types = list(c(1,1,-1), c(1,1,1), c(-1,-1,1), c(1,1,-1))
# particles2 = push(particles, types, alpha = 0.1, Df, bound = +Inf, sum_elem = 3L)

##############################################################
# Measure distance between two consecutive sets of particles #
##############################################################
delta_push = function(particles, particles2) {
  p1 = sapply(particles, c)
  p2 = sapply(particles2, c)
  sqrt(mean((p1 - p2)^2))
}

## Example
# delta_push(particles, particles2)

#################################################
# Pushing n steps and converting to 3 dim array #
#################################################
push_n = function(particles = list(c(0,0,0), c(0,0,1), c(1,0,0), c(2,0,1)), 
                  types = list(c(1,1,-1), c(1,1,1), c(-1,-1,1), c(1,1,-1)),
                  n = 100,
                  alpha = 0.1, Df, bound = +Inf, sum_elem = 3L) {
  init = array(sapply(particles, c), 
               dim = c(length(particles[[1]]), length(particles), 1))
  arrayout = init

  for(k in 1:(n-1)) {
    if(k %% 10 == 0) {
      print(k)
    }
    current = arrayout[,,dim(arrayout)[3]]
    current = lapply(1:ncol(current), function(col){current[, col]})
    to_append = push(current, types, alpha, Df, bound, sum_elem)

    to_append = sapply(to_append, c)
    arrayout = abind(arrayout, to_append, along=3)
  }
  return(arrayout)
}

delta_n = function(arrayout) {
  n = dim(arrayout)[3]
  delta = rep(NA, n-1)
  for(i in 2:n) {
    a1 = arrayout[,,i]
    a2 = arrayout[,,i-1]
    delta[i-1] = sqrt(mean((a1 - a2)^2))
  }
  return(delta)
}

## Example
# particles = list(c(0,0,0), c(0,0,1), c(1,0,0), c(2,0,1))
# types = list(c(1,1,-1), c(1,1,1), c(-1,-1,1), c(1,1,-1))
# n = 100
# alpha = 0.1
# bound = +Inf
# sum_elem = 3L
# arrayout = push_n(particles, types, n, alpha, Df, bound, sum_elem) 
# plot(delta_n(arrayout))

