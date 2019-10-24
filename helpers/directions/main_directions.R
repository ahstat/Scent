####################################################
# Get distance and normal vector between positions #
####################################################

##
# Main function
##
dn_positions = function(positions, type = "real", options = list()) {
  if(type == "real") {
    d_positions = dist_between_positions.real(positions)
    n_positions = normal_between_positions.real(positions, d_positions)
  } else if(type == "torus") {
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


# Graphical example in various 3D configurations


positions = matrix(c(0.9, 0.9, 0.3,
                     0.1, 0.2, 0,
                     0.0, 0.0, 1.2,
                     9.1, -9.1, 1.4),
                   ncol = 3, byrow = T)
plot3d(positions[,1],positions[,2],positions[,3])
text3d(positions[,1],positions[,2],positions[,3], 1:nrow(positions))
points3d(positions[,1],positions[,2],positions[,3], size = 5)
# http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization


# doable: plot 3d and add segments


markers = matrix(c(c(0,0,0), c(1,1,1)), ncol = 6, byrow = T)
open3d(scale=c(1/5,1,1))
segments3d(x=as.vector(t(markers[,c(1,4)])),
           y=as.vector(t(markers[,c(2,5)])),
           z=as.vector(t(markers[,c(3,6)])))
axes3d()
title3d(xlab="X",ylab="Y",zlab="Z")

# geometry = cbind(sep.l, sep.w, pet.l)
# plot3d(geometry[,1],geometry[,2],geometry[,3])
# text3d(geometry[,1],geometry[,2],geometry[,3],rownames(iris))
# points3d(geometry[,1],geometry[,2],geometry[,3], size = 5)

library(plot3D)
data(iris)
head(iris)
# x, y and z coordinates
x <- sep.l <- iris$Sepal.Length
y <- pet.l <- iris$Petal.Length
z <- sep.w <- iris$Sepal.Width
scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"))


library(car)
sep.l <- iris$Sepal.Length
sep.w <- iris$Sepal.Width
pet.l <- iris$Petal.Length
scatter3d(x = sep.l, y = pet.l, z = sep.w, surface=FALSE, labels = rownames(iris), id.n=nrow(iris))

