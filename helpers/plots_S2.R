###################
# Plotting on S^2 #
###################
plot_sphere = function() {
  # https://stackoverflow.com/questions/34539268
  spheres3d(0, 0, 0, lit = FALSE, color = "white")
  spheres3d(0, 0, 0, radius = 1.01, lit = FALSE, color = "black", front = "lines")
}

plot_path_on_sphere = function(traj, col = "black", radius = 0.02) {
  x <- traj[,1]
  y <- traj[,2]
  z <- traj[,3]
  spheres3d(x, y, z, col = col, radius = radius)
}

plot_point = function(A, col = "red", radius = 0.1) {
  spheres3d(A[1], A[2], A[3], col = col, radius = radius)
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
  plot_point(A, "red")
  plot_point(B, "blue")
  rm(my_matrix, A, B, t_max, theta, line_from_A_to_B)
}

plot_all_points = function(i, my_matrix, radius = 0.05) {
  plot_point(my_matrix[i,], "red", radius)
  for(j in (1:nrow(my_matrix))[-i]) {
    plot_point(my_matrix[j,], "black", radius)
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
