plot_evolution = function(Evolution, step_min = 1, step_max = NA, ...) {
  if(dim(Evolution)[2] == 2) {
    plot_evolution_circle(Evolution, step_min, step_max, ...)
  } else if(dim(Evolution)[2] == 3) {
    plot_evolution_sphere(Evolution, step_min, step_max)
  } else {
    stop(paste0("Evolution should live on S1 or S2, but found: S", dim(Evolution)[2]-1))
  }
}

plot_mymatrix = function(my_matrix, ...) {
  N = 1
  Evolution = array(NA, dim = c(dim(my_matrix), N))
  Evolution[,,1] = my_matrix
  plot_evolution(Evolution, step_min = 1, step_max = NA, ...)
}

###################
# Plotting on S^2 #
###################
plot_evolution_sphere = function(Evolution, step_min, step_max) {
  if(is.na(step_max)) {
    step_max = dim(Evolution)[3]
  }
  
  plot_sphere()
  for(step in seq(from = step_min, to = step_max)){
    step = floor(step)
    colpalette = c("red", "blue", "green", "yellow", "magenta", "cyan", "orange", "darkgray")
    for(k in 1:nrow(Evolution)) {
      if(k <= length(colpalette)) {
        plot_point_on_sphere(Evolution[k,,step], colpalette[k])
      } else {
        plot_point_on_sphere(Evolution[k,,step], "black")
      }
    }
  }
}

plot_sphere = function() {
  # https://stackoverflow.com/questions/34539268
  spheres3d(0, 0, 0, lit = FALSE, color = "white")
  spheres3d(0, 0, 0, radius = 1.01, lit = FALSE, color = "black", front = "lines")
}

plot_point_on_sphere = function(A, col = "red", radius = 0.1) {
  spheres3d(A[1], A[2], A[3], col = col, radius = radius)
}

plot_path_on_sphere = function(traj, col = "black", radius = 0.02) {
  # traj is a general trajectory
  x <- traj[,1]
  y <- traj[,2]
  z <- traj[,3]
  spheres3d(x, y, z, col = col, radius = radius)
}

if(debug) {
  my_matrix = sample_surface_sphere(n_elem = 2, dim_S = 2, seed = 1234)
  A = my_matrix[1,]
  B = my_matrix[2,]
  t_max = great_circle_distance(A, B) / 2  # t_max = 2*pi
  theta = seq(from = 0, to = t_max, length.out = 100)
  line_from_A_to_B = t(sapply(theta, function(t) {rotated(A, B, t)}))
  plot_sphere()
  plot_path_on_sphere(line_from_A_to_B)
  plot_point_on_sphere(A, "red")
  plot_point_on_sphere(B, "blue")
  rm(my_matrix, A, B, t_max, theta, line_from_A_to_B)
}

plot_all_points_on_sphere = function(i, my_matrix, radius = 0.05) {
  plot_point_on_sphere(my_matrix[i,], "red", radius)
  for(j in (1:nrow(my_matrix))[-i]) {
    plot_point_on_sphere(my_matrix[j,], "black", radius)
  }
}

segment_R_n_func = function(A, B) {
  # on the tangent space in R^{n-1}. Not ok for segment in S^{n-1}
  theta = seq(from = 0, to = 1, length.out = 100)
  segment_R_n = t(sapply(theta, function(t) {t * A + (1-t) * B}))
  return(segment_R_n)
}

segment_S_n_func = function(A, B, t_max = "segment") {
  if(t_max == "line") {
    t_max = 2*pi 
  } else if(t_max == "segment") {
    t_max = great_circle_distance(A, B)
  } else if(t_max == "semisegment") {
    t_max = great_circle_distance(A, B) / 2
  }
  theta = seq(from = 0, to = t_max, length.out = 100)
  line_from_A_to_B = t(sapply(theta, function(t) {rotated(A, B, t)}))
  return(line_from_A_to_B)
}

plot_segment_R_n = function(A, B, col = "black") {
  segment_R_n = segment_R_n_func(A, B)
  plot_path_on_sphere(segment_R_n, col = col)
}

plot_segment_S_n = function(A, B, col = "black") {
  segment_S_n = segment_S_n_func(A, B)
  plot_path_on_sphere(segment_S_n, col = col)
}

###################
# Plotting on S^1 #
###################
plot_evolution_circle = function(Evolution, step_min, step_max, ...) {
  if(is.na(step_max)) {
    step_max = dim(Evolution)[3]
  }
  
  par(mfrow = c(1,1))
  plot_circle(...)
  for(step in seq(from = step_min, to = step_max)){
    step = floor(step)
    for(k in 1:nrow(Evolution)) {
      plot_point_on_circle(Evolution[k,,step], col = k+1, 2)
    }
  }
}

plot_circle = function(...) {
  # https://stackoverflow.com/questions/22265704/drawing-circle-in-r/22266006
  # initialize a plot
  plot(c(-1, 1), c(-1, 1), type = "n", asp = 1, ...)
  
  # prepare "circle data"
  radius <- 1
  theta <- seq(0, 2 * pi, length = 200)
  
  # draw the circle
  lines(x = radius * cos(theta), y = radius * sin(theta))
}

plot_point_on_circle = function(A, col = "red", radius = 0.1) {
  points(A[1], A[2], col = col, lwd = radius)
}

plot_path_on_circle = function(traj, col = "black", radius = 2) {
  x <- traj[,1]
  y <- traj[,2]
  lines(x, y, col = col, lwd = radius)
}

plot_segment_R_2 = function(A, B, col = "black") {
  segment_R_2 = segment_R_n_func(A, B)
  plot_path_on_circle(segment_R_2, col = col)
}
