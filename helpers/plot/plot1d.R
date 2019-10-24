##################################################################
# Plot positions and directions in 1D as a particular case of 2D #
##################################################################
plot1d_positions_and_directions_debug = function(positions, length_segment = NA, type = "real", options = list()) {
  dim_space = dim(positions)[2]
  if(dim_space != 1) {
    stop("Plotting for dimension 1 data only")
  }

  positions = cbind(0, positions)
  
  if(type == "torus") {
    options$torus_dim = c(1, options$torus_dim)
  }
  
  plot2d_positions_and_directions_debug(positions, length_segment, type, options)
}