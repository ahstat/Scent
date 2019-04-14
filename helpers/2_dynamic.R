####################
# Mixture function #
####################
mix_func = function(f, particles, bound = +Inf, sum_elem = 3L) {
  dim = length(particles[[1]])
  Sigma = diag(dim)
  bounds = rep(bound, dim)
  function(x) {
    out = sapply(particles, function(particle_pos) {
      f(particle_pos, Sigma, bounds, sum_elem)(x)
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

particles = list(c(0,0,0), c(0,0,1), c(1,0,0), c(2,0,1))
types = list(c(1,1,-1), c(1,1,1), c(-1,-1,1), c(1,1,-1))
push(particles, types, alpha = 0.1, Df, bound = +Inf, sum_elem = 3L)

