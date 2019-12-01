Df = function(angle) {
  -sin(angle)
  # If density is C + cos(t), then we get -sin(t)
  # We select C such that we have a density over the hypersphere
  # If we have a sphere S(n), 
  # We integrate from a point A to the opposite pole B, and t is the
  # parallel from 0 (in A) to pi (in B). For pi/2, we are on the equator,
  # of size Vol(S(n-1))*1. If we are between equator and pole, we multiply
  # by sin(t). Since f(x) = f(t) [same value over all directions],
  # we want f such that Vol(S(n-1))*int_0^\pi f(t) sin(t) dt is 1
  # 
  # In our example, int_0^\pi cos(t) sin(t) dt = 0 so
  # Vol(S(n-1))*int_0^\pi f(t) sin(t) dt = Vol(S(n-1)) * C * int_0^\pi sin(t) dt
  #                                      = Vol(S(n-1)) * C * 2 = 1
  # So: C = 1 / (4 * Vol(S(n-1))) = Gamma(n/2) / (4*pi^(n/2))
  #
  # t = seq(from = -pi, to = pi, length.out = 100)
  # plot(t, 1 + cos(t), ylim = c(-1, 2))
  # lines(t, -sin(t), col = "red")
  #
  # Maybe more natural densities
}

matrix_of_distances = function(my_matrix) {
  M = matrix(NA, nrow = nrow(my_matrix), ncol = nrow(my_matrix))
  for(i in 1:(nrow(my_matrix)-1)) {
    for(j in (i+1):nrow(my_matrix)) {
      M[i, j] = great_circle_distance(my_matrix[i,], my_matrix[j,])
    }
  }
  return(M)
}

matrix_of_D_distances = function(my_matrix, Df) {
  M = matrix_of_distances(my_matrix)
  M[] <- vapply(M, Df, numeric(1))
  M[lower.tri(M)] = t(M)[lower.tri(M)]
  return(M)
}

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

# To see: GMM in this case, how to get the natural combination with a distance alpha
