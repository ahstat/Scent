####################
# Mixture function #
####################
mix_func = function(f, particles, pre_where_evaluate = pre_where_evaluate_func(c(+Inf, +Inf, +Inf), 0L)) {
  function(x) {
    out = sapply(particles, function(particle_pos) {
      f(particle_pos, pre_where_evaluate)(x)
    })
    if(is.null(dim(out)[1])) { # 1D = Evaluation of a density
      return(mean(out))
    } else { # 2D or more = Evaluation of the derivative
      return(apply(out, 1, mean))
    }
  }
}

# Examples
particles = list(c(0,0,0), c(0,0,1), c(1,0,0), c(2,0,1))
bounds = c(+Inf, +Inf, +Inf)
sum_elem = 3L
pre_where_evaluate = pre_where_evaluate_func(bounds, sum_elem)
mix_func(f, particles, pre_where_evaluate)(c(0,0,0))
mix_func(f, particles, pre_where_evaluate)(c(0,0,1))
mix_func(f, particles, pre_where_evaluate)(c(1,0,0))
mix_func(f, particles, pre_where_evaluate)(c(2,0,1))
mix_func(Df, particles, pre_where_evaluate)(c(0,0,0))
mix_func(Df, particles, pre_where_evaluate)(c(0,0,1))
mix_func(Df, particles, pre_where_evaluate)(c(1,0,0))
mix_func(Df, particles, pre_where_evaluate)(c(2,0,1))

particles = list(c(0,0,0), c(0,0,1))
bound = c(+Inf, +Inf, +Inf)
sum_elem = 3L
pre_where_evaluate = pre_where_evaluate_func(bounds, sum_elem)
mix_func(f, particles, pre_where_evaluate)(c(0,0,0))
mix_func(f, particles, pre_where_evaluate)(c(0,0,1))
mix_func(Df, particles, pre_where_evaluate)(c(0,0,0))
mix_func(Df, particles, pre_where_evaluate)(c(0,0,1))

## Representation of each position inside \prod_i [0, bound_i]
reduce_into_canonical_repr = function(position, bounds) {
  position0 = position
  for(i in 1:length(bounds)) {
    if(!is.infinite(bounds[i])) {
      position0[i] = position[i] %% bounds[i]
    } else {
      position0[i] = position[i]
    }
  }
  return(position0)
}

####################
# Pushing one step #
####################
push = function(particles, types, alpha = 0.1, Df, 
                pre_where_evaluate = pre_where_evaluate_func(+Inf, 3L), bounds) {
  
  particles = lapply(particles, reduce_into_canonical_repr, bounds)
    
  dim_cur = length(particles[[1]])
  Dg = mix_func(Df, particles, pre_where_evaluate)
  matrix_mix_applied_in_each_part = sapply(particles, Dg) # matrix of shape (dim, nb_particles) 
  matrix_types = sapply(lapply(types, function(x){rep(x, dim_cur)}), c)
  
  out = sapply(particles, c) + matrix_types * alpha * matrix_mix_applied_in_each_part
  if(dim_cur > 1) {
    out = lapply(1:ncol(out), function(col){out[, col]})
  } else {
    out = lapply(out, function(x){x})
  }
  return(out)
}

## Example
particles = list(c(0,0,0), c(0,0,1), c(1,0,0), c(2,0,1))
types = c(-1, 1, -1, 1)
particles2 = push(particles, types, alpha = 0.1, Df, 
                  pre_where_evaluate_func(c(+Inf, +Inf, +Inf), 3L),
                  c(+Inf, +Inf, +Inf))

## Example
particles = list(c(0), c(2.5))
types = c(1, 1)
alpha = 0.1
pre_where_evaluate = pre_where_evaluate_func(c(5), 10L)
push(particles, types, alpha, Df, pre_where_evaluate, c(5))

xmax = 10
pre_where_evaluate = pre_where_evaluate_func(xmax, 10L)
x = seq(from = 0, to = xmax, length.out = 100)
fx = sapply(x, mix_func(f, particles, pre_where_evaluate))
plot(x, fx)

##############################################################
# Measure distance between two consecutive sets of particles #
##############################################################
# delta_push = function(particles, particles2) {
#   p1 = sapply(particles, c)
#   p2 = sapply(particles2, c)
#   sqrt(mean((p1 - p2)^2))
# }

## Example
# delta_push(particles, particles2)

#################################################
# Pushing n steps and converting to 3 dim array #
#################################################
push_n = function(particles = list(c(0,0,0), c(0,0,1), c(1,0,0), c(2,0,1)), 
                  types = c(1,1,-1,1),
                  n = 100,
                  alpha = 0.1, Df, bounds = c(+Inf, +Inf, +Inf), sum_elem = 3L) {
  init = array(sapply(particles, c), 
               dim = c(length(particles[[1]]), length(particles), 1))
  arrayout = init
  
  pre_where_evaluate = pre_where_evaluate_func(bounds, sum_elem)

  for(k in 1:(n-1)) {
    if(k %% 10 == 0) {
      print(k)
    }
    current = arrayout[,,dim(arrayout)[3]]
    if(is.null(dim(current)[1])) {
      current = t(as.matrix(current))
    }
    current = lapply(1:ncol(current), function(col){current[, col]})
    to_append = push(current, types, alpha, Df, pre_where_evaluate, bounds)

    to_append = sapply(to_append, c)
    if(is.null(dim(to_append)[1])) {
      to_append = t(as.matrix(to_append))
    }
    arrayout = abind(arrayout, to_append, along=3)
  }
  rownames(arrayout) = NULL
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
particles = list(c(0,0,0), c(0,0,1), c(1,0,0), c(2,0,1))
types = c(1,1,-1,1)
n = 100
alpha = 0.1
bound = c(+Inf, +Inf, +Inf)
sum_elem = 3L
arrayout = push_n(particles, types, n, alpha, Df, bound, sum_elem)
plot(delta_n(arrayout))

## Example
particles = list(c(0), c(1.1), c(1.1), c(2))
types = c(1,-1,-1,1)
n = 1000
alpha = 0.1
bounds = c(3)
sum_elem = 10L
arrayout = push_n(particles, types, n, alpha, Df, bounds, sum_elem)
#plot(delta_n(arrayout))

plot(arrayout[1,1,], ylim = c(0, 3), type = "l", col = "red")
lines(arrayout[1,2,])
lines(arrayout[1,3,], col = "blue")
lines(arrayout[1,4,], col = "orange")


pre_where_evaluate = pre_where_evaluate_func(bounds, sum_elem)
plot_func_1D_onestep = function(arrayout, t, pre_where_evaluate) {
  current = arrayout[,,t]
  current = lapply(current, function(x){x})
  mix_func_current = mix_func(f, current, pre_where_evaluate)
  x = seq(from = 0, to = bounds, length.out = 100)
  fx = sapply(x, mix_func_current)
  f_current = sapply(current, mix_func_current)
  plot(x, fx, type = "l")
  points(current, f_current, col = "red")
}

t = 900
plot_func_1D_onestep(arrayout, t, pre_where_evaluate)
