# mean_tangent[i,] is the movement for point i, as seen in the tangent space of i
mean_tangent = t(sapply(1:nrow(M_logS_weighted), function(i){apply(M_logS_weighted[i,,], 2, mean)}))

# output_points[i,] is the movement for point i, as seen on the sphere
alpha = 1 # rate of move
types = c(-1, 1, -1)


push = function(my_matrix, Df, types, density_types, alpha) {
  M_logS_weighted = matrix_of_weighted_contribution(my_matrix, Df)
  
  density_types = c(-1,1,-1)
  M_logS_weighted = sweep(M_logS_weighted, 2, density_types, '*')
  #M_logS_weighted
  
  pushed_my_matrix_on_tangent = matrix(NA, ncol = ncol(my_matrix), nrow = nrow(my_matrix))
  for(i in 1:nrow(my_matrix)) {
    pushed_my_matrix_on_tangent[i,] = types[i]*alpha*apply(M_logS_weighted[i,,], 2, mean)
  }
  pushed_my_matrix = matrix(NA, ncol = ncol(my_matrix), nrow = nrow(my_matrix))
  for(i in 1:nrow(my_matrix)) {
    pushed_my_matrix[i,] = ExpS(my_matrix[i,], pushed_my_matrix_on_tangent[i,])
  }
  
  
}

output_points = t(sapply(1:nrow(M_logS_weighted), function(i){
  ExpS(my_matrix[i,], types[i]*alpha*apply(M_logS_weighted[i,,], 2, mean))
}))