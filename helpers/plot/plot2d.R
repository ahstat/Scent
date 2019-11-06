#######################################
# Plot positions and directions in 2D #
#######################################
plot2d_positions_and_directions_debug = function(positions, length_segment = NA, manifold = "real", options) {
  dim_space = dim(positions)[2]
  if(dim_space != 2) {
    stop("Plotting for dimension 2 data only")
  }
  nb_part = dim(positions)[1]
  if(nb_part <= 1) {
    stop("Plotting for at least 2 positions")
  }
  
  dn_pos = dn_positions(positions, manifold, options)
  
  if(manifold == "real") {
    positions0 = positions
    xlim = range(positions0[,1]*1.1)
    ylim = range(positions0[,2]*1.1)
  } else if(manifold == "torus") {
    positions0 = reduce_into_canonical_repr(positions, options$torus_dim)
    xlim = range(c(0, options$torus_dim[1]))
    ylim = range(c(0, options$torus_dim[2]))
  }
  
  plot(positions0[,1], positions0[,2], asp = 1, xlim = xlim, ylim = ylim, xlab = "x", ylab = "y")
  text(positions0[,1], positions0[,2], 1:nrow(positions0), cex = 0.7, pos = 3)
  
  for(i in 1:nrow(positions0)) {
    for(j in (1:nrow(positions0))[-i]) {
      my_segment = segment_from_to(i, j, length_segment, positions0, dn_pos, manifold, options)
      lines(my_segment)
    }
  }
}

# Helper for `plot_positions_and_directions`
segment_from_to = function(i, j, length_segment = NA, positions0, dn_pos, manifold = "real", options) {
  if(is.na(length_segment)) {
    length_segment = dn_pos$d_positions[i,j]
  }
  
  segment_i_to_j = seq2d(positions0[i,], # origin of the segment
                         positions0[i,] + length_segment*dn_pos$n_positions[i,j,]) # final position of the segment
  
  if(manifold == "torus") {
    segment_i_to_j = segment_torus(segment_i_to_j, options)
  }
  
  return(segment_i_to_j)
}

# Helper for `segment_from_to`
segment_torus = function(segment_i_to_j, options) {
  reduced_segment_i_to_j = reduce_into_canonical_repr(segment_i_to_j, options$torus_dim)
  idx_insert_NA = which(apply(apply(segment_i_to_j - reduced_segment_i_to_j, 2, diff), 1, sum) != 0)
  for(idx in sort(idx_insert_NA, decreasing = TRUE)) {
    reduced_segment_i_to_j = rbind(reduced_segment_i_to_j[1:(idx),], 
                                   rep(NA, ncol(reduced_segment_i_to_j)),
                                   reduced_segment_i_to_j[(idx+1):nrow(reduced_segment_i_to_j),]) 
  }
  return(reduced_segment_i_to_j)
}

# Helper for `segment_from_to`
seq2d = function(from = c(0,0), to = c(1,1), length.out = 100) {
  sapply(1:length(from), function(x) {
    from[x] + (to[x] - from[x]) *  (0:(length.out-1))/length.out
  })
}
