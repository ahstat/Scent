.matrix1_of_weighted_contribution = function(my_matrix, g, manifold = "S") {
  n_elem = nrow(my_matrix)
  d = ncol(my_matrix) # ambiant space R^d containing the manifold: H^{d-1}, S^{d-1}, or R^d
  M_log_weighted = array(NA, dim = c(n_elem, n_elem, d))
  for(i in 1:n_elem) {
    for(j in 1:n_elem) {
      M_log_weighted[i,j,] = Log_weighted_M(manifold)(my_matrix[i,], my_matrix[j,], g)
    }
  }
  return(M_log_weighted)
  # M_log_weighted[A, B,] = weighted point B seen on the tangent space of A
}

.matrix2_of_weighted_contribution_with_densitypes = function(M_log_weighted, densitypes) {
  M_log_weighted_with_densitypes = sweep(M_log_weighted, 2, densitypes, '*')
  return(M_log_weighted_with_densitypes)
  # M_logS_weighted_with_densitypes[A, B,] = densitype + weighted point B seen on the tangent space of A
}

.matrix3_of_mean_action = function(M_log_weighted_with_densitypes) {
  n_elem = dim(M_log_weighted_with_densitypes)[1]
  d = dim(M_log_weighted_with_densitypes)[3] # ambiant space R^d containing the manifold: H^{d-1}, S^{d-1}, or R^d
  M_log_mean_action = matrix(NA, nrow = n_elem, ncol = d)
  for(i in 1:n_elem) {
    mean_action_on_i_from_others = apply(M_log_weighted_with_densitypes[i,,], 2, mean)
    M_log_mean_action[i,] = mean_action_on_i_from_others
  }
  return(M_log_mean_action)
  # M_log_mean_action[A,] = mean action on A seen on the tangent space of A
}

.matrix4_of_mean_action_with_types = function(M_log_mean_action, types, alpha = 1) {
  M_log_mean_action_with_types = M_log_mean_action * types * alpha
  # multiply row i by types[i]*alpha
  return(M_log_mean_action_with_types)
  # M_log_mean_action_with_types[A,] = mean action on A seen on the tangent space of A with ascent or descent and force
}

.matrix5_of_mean_actions_with_types_on_original_space = function(my_matrix, M_log_mean_action_with_types, manifold = "S") {
  n_elem = nrow(my_matrix)
  d = ncol(my_matrix) # ambiant space R^d containing the manifold: H^{d-1}, S^{d-1}, or R^d
  M_mean_actions_with_types_on_original_space = matrix(NA, nrow = n_elem, ncol = d)
  for(i in 1:n_elem) {
    M_mean_actions_with_types_on_original_space[i,] = Exp_M(manifold)(my_matrix[i,], M_log_mean_action_with_types[i,])
  }
  return(M_mean_actions_with_types_on_original_space)
  # M_mean_actions_with_types_on_original_space[i,] = position of A after the move
}

push = function(my_matrix, g, densitypes, types, alpha = 1, manifold = "S") {
  M1 = .matrix1_of_weighted_contribution(my_matrix, g, manifold)
  M2 = .matrix2_of_weighted_contribution_with_densitypes(M1, densitypes)
  M3 = .matrix3_of_mean_action(M2)
  M4 = .matrix4_of_mean_action_with_types(M3, types, alpha)
  M5 = .matrix5_of_mean_actions_with_types_on_original_space(my_matrix, M4, manifold)
  # For preventing numerical errors:
  if(manifold == "S") {
    M6 = t(apply(M5, 1, .normalize_me_on_S))
    # Check: apply(M6, 1, .norm_Eucl_vec)
  } else if(manifold == "H") {
    M6 = t(apply(M5, 1, .normalize_me_hyperbolic))
  } else if(manifold == "E") {
    M6 = M5
  } else {
    stop("Manifold should be 'E', 'S' or 'H'")
  }
  return(M6)
}

get_evol = function(N, my_matrix, g, densitypes, types, alpha, manifold = "S") {
  if(N < 1L) {
    stop("N is the number of steps in the evolution, should be a positive integer")
  }
  if(length(densitypes) != dim(my_matrix)[1]) {
    stop("Incorrect number of densitypes")
  }
  if(length(types) != dim(my_matrix)[1]) {
    stop("Incorrect number of types")
  }

  Evolution = array(NA, dim = c(dim(my_matrix), N))
  Evolution[,,1] = my_matrix
  if(N > 1L) {
    for(i in 2:N) {
      Evolution[,,i] = push(Evolution[,,i-1], g, densitypes, types, alpha, manifold)
    }
  }
  return(Evolution)
}

##
# Functions for unit testing only
##

## Testing push with Euclidian (direct formula with 2 loops)
.test_move_Euclidian = function(my_matrix, g, densitypes, types, alpha) {
  n = nrow(my_matrix)
  my_matrix_E_formula = matrix(rep(NA, length(my_matrix)), ncol = ncol(my_matrix))
  for(i in 1:n) {
    sum_j_1_n = 0
    for(j in 1:n) {
      vect_diff_j_i = my_matrix[j, ] - my_matrix[i, ]
      dist_i_j = sqrt(sum((vect_diff_j_i)^2))
      if(dist_i_j > 0) {
        sum_j_1_n = sum_j_1_n + densitypes[j] * (g(dist_i_j) / dist_i_j) * (vect_diff_j_i)
      }
    }
    my_matrix_E_formula[i, ] = my_matrix[i, ] - alpha * (- types[i] * n^(-1) * sum_j_1_n)
  }
  return(my_matrix_E_formula)
}

## Testing push with any manifold (direct formula with 2 loops)
.test_move_Manifold = function(my_matrix, g, densitypes, types, alpha, manifold) {
  n = nrow(my_matrix)
  my_matrix_M_formula = matrix(rep(NA, length(my_matrix)), ncol = ncol(my_matrix))
  for(i in 1:n) {
    A = my_matrix[i, ]
    sum_j_1_n = 0
    for(j in 1:n) {
      B = my_matrix[j, ]
      sum_j_1_n = sum_j_1_n + densitypes[j] * Log_weighted_M(manifold)(A, B, g)
    }
    F_i = types[i] * n^(-1) * sum_j_1_n
    my_matrix_M_formula[i, ] = Exp_M(manifold)(A, alpha * F_i)
  }
  return(my_matrix_M_formula)
}
