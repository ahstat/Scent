##
# Relations between N, alpha, Tmax
##
get_Tmax = function(alpha, N) {
  Tmax = alpha * N
  return(Tmax)
}

get_N = function(alpha, Tmax) {
  # for reference only, N may not be an integer
  N = Tmax / alpha
  return(N)
}

get_alpha = function(N, Tmax) {
  alpha = Tmax / N
  return(alpha)
}

##
# Control function
##
summary_func = function(Evolution, manifold = "E", alpha = 1) {
  N = dim(Evolution)[3]
  Tmax = get_Tmax(alpha, N)
  diffposition = diffposition_func(Evolution, manifold)
  velocity = velocity_func(Evolution, manifold, alpha)
  acceleration = acceleration_func(Evolution, manifold, alpha)
  return(list(Evolution = Evolution,
              diffposition = diffposition,
              velocity = velocity,
              acceleration = acceleration,
              alpha = alpha,
              N = N,
              Tmax = Tmax,
              manifold = manifold))
}

##
# Helpers
##
diffposition_func = function(Evolution, manifold = "E") {
  N = dim(Evolution)[3]
  n = dim(Evolution)[1]

  diffpos_func = function(i, j, k) {
    # i = one particle
    # j = another particle
    # k = step in the evolution
    dist_M(Evolution[i,,k], Evolution[j,,k], manifold)
  }

  # https://stackoverflow.com/questions/25476526
  v <- expand.grid(1:n, 1:n, 1:N)
  out = do.call(mapply, c(diffpos_func, unname(v)))
  distEvolution = array(out, dim = c(n, n, N))
  colnames(distEvolution) = paste0("part", 1:n)
  rownames(distEvolution) = paste0("part", 1:n)

  # # Old version
  # matrix_of_distances = function(my_matrix) {
  #   # do a quick version
  #   M = matrix(NA, nrow = nrow(my_matrix), ncol = nrow(my_matrix))
  #   for(i in 1:(nrow(my_matrix)-1)) {
  #     for(j in (i+1):nrow(my_matrix)) {
  #       M[i, j] = dist_M(my_matrix[i,], my_matrix[j,], manifold)
  #     }
  #   }
  #   return(M)
  # }
  # distEvolution2 = array(NA, dim = c(n, n, N))
  # for(k in 1:N) {
  #   distEvolution2[,,k] = matrix_of_distances(Evolution[,,k])
  # }

  return(distEvolution)
}

velocity_func = function(Evolution, manifold = "E", alpha = 1) {
  N = dim(Evolution)[3]
  n = dim(Evolution)[1]

  velo_func = function(i, k) {
    # i = step in the evolution
    # k = particule
    dist_M(Evolution[k,,i], Evolution[k,,i-1], manifold)
  }

  if(N < 2) {
    # velocity cannot be computed in this case
    velocity = matrix(NA, nrow = N, ncol = n)
    colnames(velocity) = paste0("part", 1:n)
    return(velocity)
  }

  # https://stackoverflow.com/questions/25476526
  v <- expand.grid(2:N, 1:n)
  out = do.call(mapply, c(velo_func, unname(v)))
  velocity = matrix(out, nrow = N-1, ncol = n)
  velocity = rbind(rep(NA, n), velocity)
  velocity = data.frame(velocity)
  colnames(velocity) = paste0("part", 1:n)

  # # Old version
  # # > 10 times slower:
  # velocity2 = as.data.frame(matrix(rep(NA, N*n), nrow = N, ncol = n))
  # colnames(velocity2) = paste0("part", 1:n)
  # for(i in 2:N) {
  #   for(k in 1:n) {
  #     velocity2[i, k] = dist_M(Evolution[k,,i], Evolution[k,,i-1], manifold)
  #   }
  # }

  velocity = velocity / alpha
  return(velocity)
}

acceleration_func = function(Evolution, manifold = "E", alpha = 1) {
  N = dim(Evolution)[3]
  if(N < 3) {
    # acceleration cannot be computed in this case
    acceleration = matrix(NA, nrow = N, ncol = n)
    colnames(acceleration) = paste0("part", 1:n)
    return(acceleration)
  }

  n = dim(Evolution)[1]
  velocity = velocity_func(Evolution, manifold, alpha)
  acceleration = apply(velocity, 2, diff)
  acceleration = data.frame(rbind(rep(NA, n), acceleration))

  # # Old version
  # # > 10 times slower:
  # N = dim(Evolution)[3]
  # n = dim(Evolution)[1]
  # acceleration2 = as.data.frame(matrix(rep(NA, N*n), nrow = N, ncol = n))
  # colnames(acceleration2) = paste0("part", 1:n)
  # for(i in 3:N) {
  #   for(k in 1:n) {
  #     velocity_i = dist_M(Evolution[k,,i], Evolution[k,,i-1], manifold)
  #     velocity_i_minus_1 = dist_M(Evolution[k,,i-1], Evolution[k,,i-2], manifold)
  #     acceleration2[i, k] = velocity_i - velocity_i_minus_1
  #   }
  # }

  acceleration = acceleration / alpha
  return(acceleration)
}
