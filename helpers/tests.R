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

test5 = function(seed = 0, types = c(1, 1, 1), densitypes = c(1, 1, 1), alpha = 1) {
  
  my_matrix = sample_surface_sphere(n_elem = 3, dim_S = 2, seed = seed)
  # types = rep(+1, nrow(my_matrix)) # whether ascent and descent on the global mixture function
  # densitypes = rep(+1, nrow(my_matrix)) # whether common and anti density
  # alpha = 1
  
  if(seed == 0) {
    ## Custom change 0
    A = c(0, 1, 0)
    B = normalize_me(c(0, 0.9, 0.5))
    C = c(1, 0, 0)
    my_matrix[1,] = A
    my_matrix[2,] = B
    my_matrix[3,] = C
  } else if(seed == 1) {
    ## Custom change 1
    A = c(0, 1, 0)
    B = c(0, 0, 1)
    C = c(1, 0, 0)
    my_matrix[1,] = A
    my_matrix[2,] = B
    my_matrix[3,] = C
  }
  
  A = my_matrix[1,]
  B = my_matrix[2,]
  C = my_matrix[3,]
  colA = "red"
  colB = "blue"
  colC = "green"
  
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)
  
  # row = point 0 of the tangent space
  # col = outside point as seen on the tangent
  M1 = matrix1_of_weighted_contribution(my_matrix, Df)
  Bprim_onA = M1[1, 2,]
  Cprim_onA = M1[1, 3,]
  Aprim_onB = M1[2, 1,]
  Cprim_onB = M1[2, 3,]
  Aprim_onC = M1[3, 1,]
  Bprim_onC = M1[3, 2,]
  
  great_circle_distance(A, C)
  great_circle_distance(B, C)
  
  plot_segment_R_n(A, A + Bprim_onA, col = colB)
  plot_segment_R_n(A, A + Cprim_onA, col = colC)
  plot_segment_R_n(B, B + Aprim_onB, col = colA)
  plot_segment_R_n(B, B + Cprim_onB, col = colC)
  plot_segment_R_n(C, C + Aprim_onC, col = colA)
  plot_segment_R_n(C, C + Bprim_onC, col = colB)
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  
  densitypes = c(1, 1, -1)
  M2 = matrix2_of_weighted_contribution_with_densitypes(M1, densitypes)
  Bprim_onA = M2[1, 2,]
  Cprim_onA = M2[1, 3,]
  Aprim_onB = M2[2, 1,]
  Cprim_onB = M2[2, 3,]
  Aprim_onC = M2[3, 1,]
  Bprim_onC = M2[3, 2,]
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)  
  plot_segment_R_n(A, A + Bprim_onA, col = colB)
  plot_segment_R_n(A, A + Cprim_onA, col = colC)
  plot_segment_R_n(B, B + Aprim_onB, col = colA)
  plot_segment_R_n(B, B + Cprim_onB, col = colC)
  plot_segment_R_n(C, C + Aprim_onC, col = colA)
  plot_segment_R_n(C, C + Bprim_onC, col = colB)
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  
  M3 = matrix3_of_mean_action(M2)
  Bprim_onA = M2[1, 2,]
  Cprim_onA = M2[1, 3,]
  Aprim_onB = M2[2, 1,]
  Cprim_onB = M2[2, 3,]
  Aprim_onC = M2[3, 1,]
  Bprim_onC = M2[3, 2,]
  mean_onA = M3[1,]
  mean_onB = M3[2,]
  mean_onC = M3[3,]
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)
  plot_segment_R_n(A, A + Bprim_onA, col = colB)
  plot_segment_R_n(A, A + Cprim_onA, col = colC)
  plot_segment_R_n(B, B + Aprim_onB, col = colA)
  plot_segment_R_n(B, B + Cprim_onB, col = colC)
  plot_segment_R_n(C, C + Aprim_onC, col = colA)
  plot_segment_R_n(C, C + Bprim_onC, col = colB)
  plot_segment_R_n(A, A + mean_onA, col = "goldenrod")
  plot_segment_R_n(B, B + mean_onB, col = "goldenrod")
  plot_segment_R_n(C, C + mean_onC, col = "goldenrod")
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  
  types = c(1,1,-1)
  M4 = matrix4_of_mean_action_with_types(M3, types, alpha)
  Bprim_onA = M2[1, 2,]
  Cprim_onA = M2[1, 3,]
  Aprim_onB = M2[2, 1,]
  Cprim_onB = M2[2, 3,]
  Aprim_onC = M2[3, 1,]
  Bprim_onC = M2[3, 2,]
  mean_onA = M4[1,]
  mean_onB = M4[2,]
  mean_onC = M4[3,]
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)
  plot_segment_R_n(A, A + Bprim_onA, col = colB)
  plot_segment_R_n(A, A + Cprim_onA, col = colC)
  plot_segment_R_n(B, B + Aprim_onB, col = colA)
  plot_segment_R_n(B, B + Cprim_onB, col = colC)
  plot_segment_R_n(C, C + Aprim_onC, col = colA)
  plot_segment_R_n(C, C + Bprim_onC, col = colB)
  plot_segment_R_n(A, A + mean_onA, col = "darkgoldenrod")
  plot_segment_R_n(B, B + mean_onB, col = "darkgoldenrod")
  plot_segment_R_n(C, C + mean_onC, col = "darkgoldenrod")
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  
  my_matrix_pushed = matrix5_of_mean_actions_with_types_on_sphere(my_matrix, M4)
  mean_onA = M4[1,]
  mean_onB = M4[2,]
  mean_onC = M4[3,]
  mean_onA_sphere = my_matrix_pushed[1,]
  mean_onB_sphere = my_matrix_pushed[2,]
  mean_onC_sphere = my_matrix_pushed[3,]
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)
  plot_segment_R_n(A, A + mean_onA, col = colA)
  plot_segment_R_n(B, B + mean_onB, col = colB)
  plot_segment_R_n(C, C + mean_onC, col = colC)
  plot_segment_S_n(A, mean_onA_sphere, col = paste0("dark", colA))
  plot_segment_S_n(B, mean_onB_sphere, col = paste0("dark", colB))
  plot_segment_S_n(C, mean_onC_sphere, col = paste0("dark", colC))
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  plot_point(mean_onA_sphere, paste0("dark", colA))
  plot_point(mean_onB_sphere, paste0("dark", colB))
  plot_point(mean_onC_sphere, paste0("dark", colC))
}

test6 = function(first_one = TRUE) {
  n_elem = 3
  dim_S = 1
  my_matrix = sample_surface_sphere(n_elem, dim_S)
  A = c(Re(exp(1i*0)), Im(exp(1i*0)))
  B = c(Re(exp(1i*(2/3)*pi)), Im(exp(1i*(2/3)*pi)))
  C = c(Re(exp(-1i*(2/3)*pi)), Im(exp(-1i*(2/3)*pi)))
  my_matrix[1,] = A
  my_matrix[2,] = B
  my_matrix[3,] = C
  
  if(first_one) {
    types = c(1, 1, -1)
    densitypes = c(1, -1, -1)
  } else {
    # Second one
    types = c(-1, -1, -1)
    densitypes = c(-1, 1, -1)
  }
  
  alpha = 0.1
  N = 100
  Evolution = get_evol(my_matrix, N, Df, densitypes, types, alpha)
  distEvolution = dist_evol(Evolution)
  plot_dist_evol(distEvolution)
  plot_evolution(Evolution, 1, N)
  
  distEvolution[,,dim(distEvolution)[3]]
}
