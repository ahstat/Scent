Df = function(angle) {
  -sin(angle)
}

matrix1_of_weighted_contribution = function(my_matrix, Df) {
  M_logS_weighted = array(NA, dim = c(nrow(my_matrix), nrow(my_matrix), ncol(my_matrix)))
  for(i in 1:(nrow(my_matrix))) {
    for(j in (1:nrow(my_matrix))) {
      M_logS_weighted[i,j,] = LogS_weighted(my_matrix[i,], my_matrix[j,], Df)
    }
  }
  return(M_logS_weighted)
  # M_logS_weighted[A, B,] = weighted point B seen on the tangent space of A
}

matrix2_of_weighted_contribution_with_densitypes = function(M_logS_weighted, densitypes) {
  M_logS_weighted_with_densitypes = sweep(M_logS_weighted, 2, density_types, '*')
  return(M_logS_weighted_with_densitypes)
  # M_logS_weighted_with_densitypes[A, B,] = densitype+weighted point B seen on the tangent space of A
}

matrix3_of_mean_action = function(M_logS_weighted_with_densitypes) {
  M_logS_mean_action = matrix(NA, nrow = dim(M_logS_weighted_with_densitypes)[1], ncol = dim(M_logS_weighted_with_densitypes)[3])
  for(i in 1:(nrow(M_logS_weighted_with_densitypes))) {
    mean_action_on_i_from_others = apply(M_logS_weighted_with_densitypes[i,,], 2, mean)
    M_logS_mean_action[i,] = mean_action_on_i_from_others
  }
  return(M_logS_mean_action)
  # M_logS_mean_action[A,] = mean action on A seen on the tangent space of A
}

matrix4_of_mean_action_with_types = function(M_logS_mean_action, types, alpha = 1) {
  M_logS_mean_action_with_types = M_logS_mean_action * types * alpha
  # multiply row i by types[i]*alpha
  return(M_logS_mean_action_with_types)
  # M_logS_mean_action_with_types[A,] = mean action on A seen on the tangent space of A with ascent or descent and force
}

matrix5_of_mean_actions_with_types_on_sphere = function(my_matrix, M_logS_mean_action_with_types) {
  M_mean_actions_with_types_on_sphere = matrix(NA, nrow = nrow(my_matrix), ncol = ncol(my_matrix))
  for(i in 1:(nrow(my_matrix))) {
    M_mean_actions_with_types_on_sphere[i,] = ExpS(my_matrix[i,], M_logS_mean_action_with_types[i,])
  }
  return(M_mean_actions_with_types_on_sphere)
  # M_mean_actions_with_types_on_sphere[i,] = position of A after the move
}

push = function(my_matrix, Df, densitypes, types, alpha = 1) {
  M1 = matrix1_of_weighted_contribution(my_matrix, Df)
  M2 = matrix2_of_weighted_contribution_with_densitypes(M1, densitypes)
  M3 = matrix3_of_mean_action(M2)
  M4 = matrix4_of_mean_action_with_types(M3, types, alpha)
  M5 = matrix5_of_mean_actions_with_types_on_sphere(my_matrix, M4)
  return(M5)
}






# matrix_of_distances = function(my_matrix) {
#   M = matrix(NA, nrow = nrow(my_matrix), ncol = nrow(my_matrix))
#   for(i in 1:(nrow(my_matrix)-1)) {
#     for(j in (i+1):nrow(my_matrix)) {
#       M[i, j] = great_circle_distance(my_matrix[i,], my_matrix[j,])
#     }
#   }
#   return(M)
# }
# 
# matrix_of_D_distances = function(my_matrix, Df) {
#   M = matrix_of_distances(my_matrix)
#   M[] <- vapply(M, Df, numeric(1))
#   M[lower.tri(M)] = t(M)[lower.tri(M)]
#   return(M)
# }

# my_matrix = sample_surface_sphere(n_elem = 3, dim_S = 2, seed = 1234)
# plot(sort(matrix_of_distances(my_matrix)), ylim = c(0, pi))
# plot((abs(sort(matrix_of_D_distances(my_matrix, Df)))))
# M = matrix_of_D_distances(my_matrix, Df)
# A = my_matrix[1,]
# B = my_matrix[2,]
# C = my_matrix[3,]
# rotated(A, B, M[1,2])
# rotated(A, C, M[1,3])
# 
# B_prim = deriv_rotated(A, B)
# C_prim = deriv_rotated(A, C)
# rotated_from_derivative(A, B_prim, M[1,2])
# rotated_from_derivative(A, C_prim, M[1,3])

# deriv_rotated_array = function(my_matrix) {
#   deriv_M = array(NA, dim = c(nrow(my_matrix), nrow(my_matrix), ncol(my_matrix)))
#   for(i in 1:(nrow(my_matrix))) {
#     for(j in (1:nrow(my_matrix))[-i]) {
#       deriv_M[i,j,] = deriv_rotated(my_matrix[i,], my_matrix[j,])
#     }
#   }
#   return(deriv_M)
# }
# 
# get_mean_tangent_with_weights_from_i = function(i, dir_points, my_matrix_derivdist, my_matrix_deriv) {
#   N = nrow(my_matrix_derivdist)
#   each_tangent = sapply(1:N, function(j){dir_points[j]*my_matrix_derivdist[i,j]*my_matrix_deriv[i,j,]})
#   each_tangent = each_tangent[,-i]
#   mean_tangent = apply(each_tangent, 1, sum)
#   mean_tangent = mean_tangent / N
#   return(mean_tangent)
# }