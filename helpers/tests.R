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
  plot_point(A, "red")
  plot_point(B, "blue")
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
  plot_point(A, "red")
  plot_point(B, "blue")
  
  Lambda = c(crossprod(A, B))
  B_prim = deriv_rotated(A, B) # necessary to live on the tangent space then
  plot_point(B_prim, "green")
  
  Tangent = list()
  t_vec = seq(from = 0.1, to = 1, by = 0.1)
  for(i in 1:length(t_vec)) {
    Tangent[[i]] = A + t_vec[i] * B_prim
    plot_point(Tangent[[i]], "orange")
  }
}

test4 = function(seed = 1) {
  my_matrix = sample_surface_sphere(n_elem = 10, dim_S = 2, seed = seed)
  #types = rep(+1, nrow(my_matrix)) # whether ascent and descent on the global mixture function
  types = rep(+1, nrow(my_matrix)) #c(+1, -1, -1, -1)
  densitypes = rep(+1, nrow(my_matrix)) # -1 will give you negative density for this particle
  # densitypes = c(-1,1,1)
  
  ## Change points
  #A = c(0, 1, 0)
  #B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
  #C = c(0.5, 0.9, 0); C = C / sqrt(sum(C^2))
  A = c(0, 1, 0)
  B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
  C = c(1, 0, 0)
  my_matrix[1,] = A
  my_matrix[2,] = B
  my_matrix[3,] = C
  ## End custom changes
  
  my_matrix_list = list()
  N = 25
  my_matrix_list[[1]] = my_matrix
  for(k in 2:N) {
    my_matrix_pushed = push(my_matrix_list[[k-1]], Df, densitypes, types, alpha = 0.33) 
    my_matrix_list[[k]] = my_matrix_pushed
  }
  
  i = 1
  plot_sphere()
  for(k in 1:N) {
    print(k)
    plot_all_points(i, my_matrix_list[[k]])
  }
}
