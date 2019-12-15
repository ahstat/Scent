matrix_of_distances = function(my_matrix) {
  M = matrix(NA, nrow = nrow(my_matrix), ncol = nrow(my_matrix))
  for(i in 1:(nrow(my_matrix)-1)) {
    for(j in (i+1):nrow(my_matrix)) {
      M[i, j] = great_circle_distance(my_matrix[i,], my_matrix[j,])
    }
  }
  return(M)
}

get_evol = function(my_matrix, N, Df, densitypes, types, alpha) {
  Evolution = array(NA, dim = c(dim(my_matrix), N))
  Evolution[,,1] = my_matrix
  for(i in 2:N) {
    Evolution[,,i] = push(Evolution[,,i-1], Df, densitypes, types, alpha)
  }
  return(Evolution)
}

dist_evol = function(Evolution) {
  n_elem = dim(Evolution)[1]
  N = dim(Evolution)[3]
  distEvolution = array(NA, dim = c(n_elem, n_elem, N))
  for(i in 1:N) {
    distEvolution[,,i] = matrix_of_distances(Evolution[,,i])
  }
  return(distEvolution)
}

plot_dist_evol = function(distEvolution, main_title = "") {
  par(mfrow=c(n_elem, n_elem), oma = c(5,4,0,0) + 1.5, mar = c(0,0,1,1) + 0.5)
  for(i in 1:nrow(distEvolution)) {
    for(j in 1:nrow(distEvolution)) {
      #print(paste(i, j))
      if(!all(is.na(distEvolution[i,j,]))) {
        plot(distEvolution[i,j,], 
             type = "l", 
             #xaxt='n', 
             main = paste0("(", i, ",", j, ")"))
      } else {
        plot.new()
      }
    }
  }
  title(main_title, outer = TRUE)
  par(mfrow=c(1,1))
}
