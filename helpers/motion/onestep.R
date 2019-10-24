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
      # in this case, fD should be 0...
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
move1 = function(positions, types, 
                 eps = 0.1, d, f) {
  # Distance between pairs of positions
  D = d_positions(positions, d)
  # euclidian distance... not ok
  
  # Gradient force between individuals pairs of positions (based on f(distance))
  fD = -structure(vapply(D, f, numeric(1)), dim=dim(D)) # https://stackoverflow.com/questions/8579257
  
  # Normal directions
  N = n_positions(positions, d)
  
  # Gradients by summing the individuals contributions
  # Same dimension as `positions`
  grad = grad_fn(N, fD)
  
  # Replicate types (it makes few sense to do otherwise)
  types_v = replicate(ncol(positions), types)
  
  # Update positions
  positions = positions + eps * types_v * grad
  return(list("positions" = positions, "grad" = grad))
}