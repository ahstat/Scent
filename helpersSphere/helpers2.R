deriv_rotated_array = function(my_matrix) {
  deriv_M = array(NA, dim = c(nrow(my_matrix), nrow(my_matrix), ncol(my_matrix)))
  for(i in 1:(nrow(my_matrix))) {
    for(j in (1:nrow(my_matrix))[-i]) {
      deriv_M[i,j,] = deriv_rotated(my_matrix[i,], my_matrix[j,])
    }
  }
  return(deriv_M)
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

get_mean_tangent_with_weights_from_i = function(i, dir_points, my_matrix_derivdist, my_matrix_deriv) {
  N = nrow(my_matrix_derivdist)
  each_tangent = sapply(1:N, function(j){dir_points[j]*my_matrix_derivdist[i,j]*my_matrix_deriv[i,j,]})
  each_tangent = each_tangent[,-i]
  mean_tangent = apply(each_tangent, 1, sum)
  mean_tangent = mean_tangent / N
  return(mean_tangent)
}

plot_mean_tangent_with_weights_from_i = function(i, dir_points, my_matrix_derivdist, my_matrix_deriv, my_matrix) {
  mean_tangent = get_mean_tangent_with_weights_from_i(i, dir_points, my_matrix_derivdist, my_matrix_deriv)
  t_vec = seq(from = 0.1, to = 1, by = 0.05)
  traj_tangent = t(sapply(t_vec, function(t) {my_matrix[i,] + t * mean_tangent}))
  plot_path_on_sphere(traj_tangent, "goldenrod")
}
