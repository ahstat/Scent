##############
# Distance d #
##############
# See distance.R
##
# Generalization: Take another distance, or even take it to emulate another manifold

########################
# Normalized direction #
########################
n = function(x, y) {
  # Normalized direction from x to y
  (y-x) / d(x, y)
}

#############
# Positions #
#############
# See init.R
##
# Generalization: Take complex number instead of real numbers

#####################################################
# Matrix of normalized directions between positions #
#####################################################
n_positions = function(positions) {
  # Matrix of normalized directions between two individual positions
  # Ex: n_positions(positions)[1, 2, ] is the normalized direction from 
  # position 1 to position 2
  nb_part = nrow(positions)
  out = array(NA, dim = c(nb_part, nb_part, ncol(positions)))
  for(i in 1:nb_part) {
    for(j in 1:nb_part) {
      out[i,j,] = n(positions[i,], positions[j,])
    }
  }
  return(out)
}

##############
# Ddensity f #
##############
# See ddensity.R

#############
# Gradients #
#############
grad_i = function(i, N, fD) {
  nb_part = dim(N)[1]
  nb_dim = dim(N)[3]
  idx_p_others = (1:nb_part)[-i]
  
  out = rep(0, nb_dim)
  for(j in idx_p_others) {
    out = out + N[i,j,] * fD[i,j]
  }
  out = out / nb_part
  return(out)
}

grad_fn = function(N, fD) {
  nb_part = dim(N)[1]
  nb_dim = dim(N)[3]
  out = matrix(NA, nrow = nb_part, ncol = nb_dim)
  for(i in 1:nb_part) {
    out[i,] = grad_i(i, N, fD)
  }
  return(out)
}

##########
# Motion #
##########
move1 = function(positions, types, eps = 0.1) {
  # Distance between pairs of positions
  D = rdist(positions) # https://stackoverflow.com/questions/9879608
  
  # Gradient force between individuals pairs of positions (based on f(distance))
  fD = -structure(vapply(D, f, numeric(1)), dim=dim(D)) # https://stackoverflow.com/questions/8579257

  # Normal directions
  N = n_positions(positions)
  
  # Gradients by summing the individuals contributions
  # Same dimension as `positions`
  grad = grad_fn(N, fD)
  
  # Replicate types (it makes few sense to do otherwise)
  types_v = replicate(ncol(positions), types)
  
  # Update positions
  positions = positions + eps * types_v * grad
  return(list("positions" = positions, "grad" = grad))
}

move = function(particles, steps = 100, eps = 0.1,
                print_every = 1e16) {
  positions = particles$positions
  types = particles$types

  # Moving multiple steps
  init = array(positions, 
               dim = c(dim(positions), 1))
  arrayout = init
  
  init_grad = array(NA,
                    dim = c(dim(positions), 1))
  arrayout_grad = init_grad
  
  for(k in 1:steps) {
    # print_every may be 10, 100, ... or 1e16
    if(k %% print_every == 0) {
      print(k)
    }
    current = arrayout[,,dim(arrayout)[3]]
    to_append = move1(current, types, eps)
    arrayout = abind(arrayout, to_append$positions, along=3)
    arrayout_grad = abind(arrayout_grad, to_append$grad, along=3)
  }
  return(list("positions" = arrayout, "grad" = arrayout_grad))
}
