#############
# Gradients #
#############
grad_i = function(i, N, fD) {
  nb_part = dim(N)[1]
  nb_dim = dim(N)[3]
  idx_p_others = (1:nb_part)[-i]
  
  out = rep(0, nb_dim)
  for(j in idx_p_others) {
    if(!is.nan(N[i,j,][1])) {
      out = out + N[i,j,] * fD[i,j]
    } else {
      # in this case, fD should be 0
    }
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
move1 = function(positions, types, f, manifold = "real", options = list(), eps = 0.1) {
  
  DN = dn_positions(positions, manifold, options)
  
  # Distance between pairs of positions
  D = DN$d_positions
  
  # Gradient force between individuals pairs of positions (based on f(distance))
  fD = -structure(vapply(D, f, numeric(1)), dim=dim(D)) # https://stackoverflow.com/questions/8579257
  
  # Normal directions
  N = DN$n_positions
  
  # Gradients by summing the individuals contributions
  # Same dimension as `positions`
  grad = grad_fn(N, fD)
  
  # Replicate types (it makes few sense to do otherwise)
  types_v = replicate(ncol(positions), types)
  
  # Update positions
  positions = positions + eps * types_v * grad
  return(list("positions" = positions, "grad" = grad))
}

##
# Example
##
positions = matrix(c(0.25,
                     0.75),
                   ncol = 1, byrow = T)
types = c(1, 1)
manifold = "torus"
options = list("torus_dim" = c(1))
f = f_derivdnorm1

move1(positions, types, f, manifold, options, eps = 0.1)

DN = dn_positions(positions, manifold, options)
D = DN$d_positions
N = DN$n_positions

# f is not OK for a torus.
fD = -structure(vapply(D, f, numeric(1)), dim=dim(D))



# Gradients by summing the individuals contributions
# Same dimension as `positions`
grad = grad_fn(N, fD)



plot2d_positions_and_directions_debug(positions, length_segment = NA, manifold, options)