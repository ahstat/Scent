matrix_of_distances = function(my_matrix) {
  M = matrix(NA, nrow = nrow(my_matrix), ncol = nrow(my_matrix))
  for(i in 1:(nrow(my_matrix)-1)) {
    for(j in (i+1):nrow(my_matrix)) {
      M[i, j] = great_circle_distance(my_matrix[i,], my_matrix[j,])
    }
  }
  return(M)
}

