###################
# Plotting on S^1 #
###################
plot_circle = function() {
  # https://stackoverflow.com/questions/22265704/drawing-circle-in-r/22266006
  # initialize a plot
  plot(c(-1, 1), c(-1, 1), type = "n", asp = 1)
  
  # prepare "circle data"
  radius <- 1
  theta <- seq(0, 2 * pi, length = 200)
  
  # draw the circle
  lines(x = radius * cos(theta), y = radius * sin(theta))
}

plot_path_on_circle = function(traj, col = "black", radius = 2) {
  x <- traj[,1]
  y <- traj[,2]
  lines(x, y, col = col, lwd = radius)
}

plot_point_on_circle = function(A, col = "red", radius = 0.1) {
  points(A[1], A[2], col = col, lwd = radius)
}

plot_segment_R_2 = function(A, B, col = "black") {
  segment_R_2 = segment_R_n_func(A, B)
  plot_path_on_circle(segment_R_2, col = col)
}