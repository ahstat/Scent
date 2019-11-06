####################################################
# Get distance and normal vector between positions #
####################################################

##
# Main function
##
dn_positions = function(positions, manifold = "real", options = list()) {
  if(manifold == "real") {
    d_positions = dist_between_positions.real(positions)
    n_positions = normal_between_positions.real(positions, d_positions)
  } else if(manifold == "torus") {
    # `options` should be a list containing `torus_dim` in this case
    d_positions = dist_between_positions.torus(positions, options$torus_dim)
    n_positions = normal_between_positions.torus(positions, d_positions, options$torus_dim)
  }
  return(list("d_positions" = d_positions, "n_positions" = n_positions))
}

##
# Examples
##
# Graphical example in 2D configuration
positions = matrix(c(0.9, 0.9,
                     0.1, 0.2,
                     0.0, 0.0,
                     9.1, -9.1),
                   ncol = 2, byrow = T)
dn_positions(positions, "real")
options = list("torus_dim" = c(1, 1))
dn_positions(positions, "torus", options)
round(dn_positions(positions, "real")$d_positions, 2)
round(dn_positions(positions, "torus", options)$d_positions, 2)

if(debug) {
  plot2d_positions_and_directions_debug(positions, 1, "real") # segments of length 1
  plot2d_positions_and_directions_debug(positions, 0.05, "torus", options)
  plot2d_positions_and_directions_debug(positions, NA, "torus", options) # NA for whole segments
}

rm(positions, options)

# Graphical example in 1D configuration
positions = matrix(c(0.9,
                     0.15,
                     0.0,
                     9.1),
                   ncol = 1, byrow = T)
dn_positions(positions, "real")
options = list("torus_dim" = c(1))
dn_positions(positions, "torus", options)
round(dn_positions(positions, "real")$d_positions, 2)
round(dn_positions(positions, "torus", options)$d_positions, 2)

if(debug) {
  plot1d_positions_and_directions_debug(positions, 1, "real") # segments of length 1
  plot1d_positions_and_directions_debug(positions, 0.05, "torus", options)
  plot1d_positions_and_directions_debug(positions, NA, "torus", options) # NA for whole segments
}

rm(positions, options)

# Graphical example in various 3D configurations
positions = matrix(c(0.9, 0.9, 0.3,
                     0.1, 0.2, 0,
                     0.0, 0.0, 1.2,
                     9.1, -9.1, 1.4),
                   ncol = 3, byrow = T)

manifold = "torus"
options = list("torus_dim" = c(1, 1, 1))

if(debug) {
  plot3d_positions_and_directions_debug(positions, length_segment = NA, "torus", options)
  plot3d_positions_and_directions_debug(positions[c(1,2),], length_segment = 0.4, "torus", options, 
                                        col = TRUE, plot_only_from = 1)
  plot3d_positions_and_directions_debug(positions[c(1,2),], length_segment = 0.4, "torus", options, 
                                        col = TRUE, plot_only_from = 2)
}

rm(positions, options)
