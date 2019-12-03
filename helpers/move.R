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
  M_logS_weighted_with_densitypes = sweep(M_logS_weighted, 2, densitypes, '*')
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
