radius = 0.05

###################
# Plotting on S^2 #
###################
plot_sphere = function() {
  # https://stackoverflow.com/questions/34539268
  spheres3d(0, 0, 0, lit=FALSE, color="white")
  spheres3d(0, 0, 0, radius=1.01, lit=FALSE, color="black", front="lines")
}

plot_path_on_sphere = function(traj, col = "black") {
  x <- traj[,1]
  y <- traj[,2]
  z <- traj[,3]
  spheres3d(x, y, z,col = col,radius = 0.02)
}

plot_point_on_sphere = function(A, col = "red", radius = 0.1) {
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
  plot_point_on_sphere(A, "red")
  plot_point_on_sphere(B, "blue")
  rm(my_matrix, A, B, t_max, theta, line_from_A_to_B)
}

segment_from_i_to_j = function(i = 1, j = 2, my_matrix, my_matrix_deriv) {
  t_max = great_circle_distance(my_matrix[i,], my_matrix[j,])
  theta = seq(from = 0, to = t_max, length.out = 100)
  line_from_i_to_j = t(sapply(theta, function(t) {
    rotated_from_derivative(my_matrix[i,], my_matrix_deriv[i,j,], t)
  }))
  return(line_from_i_to_j)
}

plot_all_paths_from_i = function(i, my_matrix, my_matrix_deriv) {
  for(j in (1:nrow(my_matrix))[-i]) {
    plot_path_on_sphere(segment_from_i_to_j(i, j, my_matrix, my_matrix_deriv))
  }
}

plot_all_points = function(i, my_matrix) {
  plot_point_on_sphere(my_matrix[i,], "red", radius)
  for(j in (1:nrow(my_matrix))[-i]) {
    plot_point_on_sphere(my_matrix[j,], "black", radius)
  }
}

plot_all_prim_from_i = function(i, my_matrix_deriv) {
  for(j in (1:nrow(my_matrix_deriv))[-i]) {
    plot_point_on_sphere(my_matrix_deriv[i,j,], "lightgray", radius)
  }
}

plot_all_tangent_from_i = function(i, my_matrix, my_matrix_deriv, weighted = FALSE,
                                   dir_points, my_matrix_derivdist) {
  N = nrow(my_matrix)
  if(!weighted) {
    W = rep(1, N)
  } else {
    W = dir_points*my_matrix_derivdist[i,] / N
  }
  t_vec = seq(from = 0.1, to = 1, by = 0.05)
  for(j in (1:nrow(my_matrix))[-i]) {
    traj_tangent = t(sapply(t_vec, function(t) {my_matrix[i,] + t * W[j] * my_matrix_deriv[i,j,]}))
    plot_path_on_sphere(traj_tangent, "lightgray")
  }
}

plot_mean_tangent_with_weights_from_i = function(i, dir_points, my_matrix_derivdist, my_matrix_deriv, my_matrix) {
  mean_tangent = get_mean_tangent_with_weights_from_i(i, dir_points, my_matrix_derivdist, my_matrix_deriv)
  t_vec = seq(from = 0.1, to = 1, by = 0.05)
  traj_tangent = t(sapply(t_vec, function(t) {my_matrix[i,] + t * mean_tangent}))
  plot_path_on_sphere(traj_tangent, "goldenrod")
}
