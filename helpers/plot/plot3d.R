#######################################
# Plot positions and directions in 3D #
#######################################
plot3d_positions_and_directions_debug = function(positions, length_segment = NA, manifold = "real", options,
                                                 col = FALSE,
                                                 plot_only_from = NULL) {
  dim_space = dim(positions)[2]
  if(dim_space != 3) {
    stop("Plotting for dimension 3 data only")
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
    zlim = range(positions0[,3]*1.1)
  } else if(manifold == "torus") {
    positions0 = reduce_into_canonical_repr(positions, options$torus_dim)
    xlim = range(c(0, options$torus_dim[1]))
    ylim = range(c(0, options$torus_dim[2]))
    zlim = range(c(0, options$torus_dim[3]))
  }
  
  # http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization
  plot3d(positions0[,1], positions0[,2], positions0[,3], 
         xlab = "x", ylab = "y", zlab = "z",
         xlim = xlim, ylim = ylim, zlim = zlim)
  # scatter3d(positions0[,1], positions0[,2], positions0[,3], size = 5, surface = FALSE, 
  #           xlab = "x", ylab = "y", zlab = "z",
  #           xlim = xlim, ylim = ylim, zlim = zlim)
  text3d(positions0[,1], positions0[,2], positions0[,3], 1:nrow(positions0))
  points3d(positions0[,1], positions0[,2], positions0[,3], size = 5)
  
  if(is.null(plot_only_from)) {
    the_i = 1:nrow(positions0)
  } else {
    the_i = plot_only_from
  }
  
  for(i in the_i) {
    for(j in (1:nrow(positions0))[-i]) {
      my_segment = segment_from_to(i, j, length_segment, positions0, dn_pos, manifold, options)
      idx_na = which(is.na(my_segment[,1]))
      idx_na = c(0, idx_na, nrow(my_segment)+1)
      idx_na = unique(idx_na)
      for(k in 1:(length(idx_na) - 1)) {
        markers = matrix(c(my_segment[idx_na[k]+1,], my_segment[idx_na[k+1]-1,]), nrow = 1)
        segments3d(x=as.vector(t(markers[,c(1,4)])),
                   y=as.vector(t(markers[,c(2,5)])),
                   z=as.vector(t(markers[,c(3,6)])))
        if(col) {
          points3d(x=as.vector(t(markers[,c(1)])),
                   y=as.vector(t(markers[,c(2)])),
                   z=as.vector(t(markers[,c(3)])), col = "blue")
          points3d(x=as.vector(t(markers[,c(4)])),
                   y=as.vector(t(markers[,c(5)])),
                   z=as.vector(t(markers[,c(6)])), col = "red")
        }
      }
    }
  }
}
