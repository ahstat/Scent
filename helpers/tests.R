test1 = function(seed = 1, t_max = "segment") {
  my_matrix = sample_surface_sphere(n_elem = 100, dim_S = 2, seed = 1)
  A = my_matrix[1,]
  B = my_matrix[2,]
  if(t_max == "line") {
    t_max = 2*pi 
  } else if(t_max == "segment") {
    t_max = great_circle_distance(A, B)
  } else if(t_max == "semisegment") {
    t_max = great_circle_distance(A, B) / 2
  }
  theta = seq(from = 0, to = t_max, length.out = 100)
  line_from_A_to_B = t(sapply(theta, function(t) {rotated(A, B, t)}))
  apply(line_from_A_to_B, 1, norm_Eucl_vec)
  #plot(theta, apply(line_from_A_to_B, 1, function(x){great_circle_distance(line_from_A_to_B[1,], x)}))
  
  plot_sphere()
  plot_path_on_sphere(line_from_A_to_B)
  plot_point_on_sphere(A, "red")
  plot_point_on_sphere(B, "blue")
}

test2 = function(t_max = "segment") {
  A = c(0, 1, 0)
  B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
  if(t_max == "line") {
    t_max = 2*pi 
  } else if(t_max == "segment") {
    t_max = great_circle_distance(A, B)
  } else if(t_max == "semisegment") {
    t_max = great_circle_distance(A, B) / 2
  }
  
  theta = seq(from = 0, to = t_max, length.out = 100)
  line_from_A_to_B = t(sapply(theta, function(t) {rotated(A, B, t)}))
  apply(line_from_A_to_B, 1, norm_Eucl_vec)
  plot(theta, apply(line_from_A_to_B, 1, function(x){great_circle_distance(line_from_A_to_B[1,], x)}))
  
  plot_sphere()
  plot_path_on_sphere(line_from_A_to_B)
  plot_point_on_sphere(A, "red")
  plot_point_on_sphere(B, "blue")
  
  Lambda = c(crossprod(A, B))
  B_prim = deriv_rotated(A, B) # necessary to live on the tangent space then
  plot_point_on_sphere(B_prim, "green")
  
  Tangent = list()
  t_vec = seq(from = 0.1, to = 1, by = 0.1)
  for(i in 1:length(t_vec)) {
    Tangent[[i]] = A + t_vec[i] * B_prim
    plot_point_on_sphere(Tangent[[i]], "orange")
  }
}

test3 = function(seed = 1) {
  seed = 1
  my_matrix = sample_surface_sphere(n_elem = 5, dim_S = 2, seed = seed)
  dir_points = rep(+1, nrow(my_matrix))# c(+1, +1, +1)
  #dir_points = c(+1, -1, +1, +1, -1)
  #dir_points = c(-1, +1, -1, -1, +1)
  
  ## Change points
  #A = c(0, 1, 0)
  #B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
  #C = c(0.5, 0.9, 0); C = C / sqrt(sum(C^2))
  A = c(0, 1, 0)
  B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
  C = c(1, 0, 0)
  my_matrix[1,] = A
  my_matrix[2,] = B
  # my_matrix[2,] = C
  # my_matrix[3,] = C
  # my_matrix[4,] = C
  # my_matrix[5,] = C
  ## End custom changes
  
  my_matrix_derivdist = matrix_of_D_distances(my_matrix, Df)
  my_matrix_deriv = deriv_rotated_array(my_matrix)
  
  i = 1
  plot_sphere()
  plot_all_paths_from_i(i, my_matrix, my_matrix_deriv)
  plot_all_points(i, my_matrix)
  plot_all_prim_from_i(i, my_matrix_deriv)
  plot_all_tangent_from_i(i, my_matrix, my_matrix_deriv, weighted = TRUE,
                          dir_points, my_matrix_derivdist)
  plot_mean_tangent_with_weights_from_i(i, dir_points, my_matrix_derivdist, my_matrix_deriv, my_matrix)
  
  mean_tangent = get_mean_tangent_with_weights_from_i(i, dir_points, my_matrix_derivdist, my_matrix_deriv)
  t = sqrt(sum((mean_tangent)^2))
  G = rotated_from_derivative(my_matrix[i,], normalize_me(mean_tangent), t)
  great_circle_distance(G, my_matrix[i,])
  plot_point_on_sphere(G, "goldenrod", radius)
}

