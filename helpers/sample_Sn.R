###############################################################
# Sample uniform distribution on the surface of a unit sphere #
###############################################################
sample_surface_sphere = function(n_elem, dim_S = 2, seed = 1234) {
  # Pick n_elem points on S^{n-1} (20180501)
  # http://mathworld.wolfram.com/HyperspherePointPicking.html
  set.seed(seed)
  n = dim_S + 1
  M = matrix(rnorm(n * n_elem), ncol = n)
  sample = M / sqrt(apply(M^2, 1, sum))
  return(sample)
}

##################################################
# Regular at regular distance for the S^1 circle #
##################################################
unif_circle = function(n_elem) {
  vec = (1:n_elem)-1
  vecExp = exp(1i*vec*2*pi/n_elem)
  my_matrix = cbind(Re(vecExp), Im(vecExp))
  return(my_matrix)
}

#####################################
# Regular tetrahedron on S^2 sphere #
#####################################
tetrahedron_on_sphere = function() {
  n_elem = 4
  dim_S = 2
  seed = 1
  my_matrix = sample_surface_sphere(n_elem, dim_S, seed = seed)
  A = c( sqrt(8/9),          0, -1/3)
  B = c(-sqrt(2/9),  sqrt(2/3), -1/3)
  C = c(-sqrt(2/9), -sqrt(2/3), -1/3)
  D = c(         0,          0,    1)
  my_matrix[1,] = A
  my_matrix[2,] = B
  my_matrix[3,] = C
  my_matrix[4,] = D
  return(my_matrix)
}

##################################
# Square, cube, hypercube on S^n #
##################################
square_on_Sn = function(dim_S) {
  n = dim_S + 1
  n_elem = 2 * n
  my_matrix = sample_surface_sphere(n_elem, dim_S, seed = 1)
  mat_minus = diag(n) * -1
  mat_plus = diag(n) * +1
  for(i in 1:n) {
    my_matrix[2*i-1,] = mat_minus[i,]
    my_matrix[2*i,] = mat_plus[i,]
  }
  return(my_matrix)
}

##################
# 24-cell on S^3 #
##################
vingtquatrecell = function() {
  my_matrix = sample_surface_sphere(n_elem = 24, dim_S = 3, seed = 1)
  
  my_matrix[1,] =  c(-1, -1,  0,  0)
  my_matrix[2,] =  c(-1,  1,  0,  0)
  my_matrix[3,] =  c( 1, -1,  0,  0)
  my_matrix[4,] =  c( 1,  1,  0,  0)
  
  my_matrix[5,] =  c( 0,  0, -1, -1)
  my_matrix[6,] =  c( 0,  0, -1,  1)
  my_matrix[7,] =  c( 0,  0,  1, -1)
  my_matrix[8,] =  c( 0,  0,  1,  1)
  
  my_matrix[9,] =  c(-1,  0,  0, -1)
  my_matrix[10,] = c(-1,  0,  0,  1)
  my_matrix[11,] = c( 1,  0,  0, -1)
  my_matrix[12,] = c( 1,  0,  0,  1)
  
  my_matrix[13,] = c( 0, -1, -1,  0)
  my_matrix[14,] = c( 0, -1,  1,  0)
  my_matrix[15,] = c( 0,  1, -1,  0)
  my_matrix[16,] = c( 0,  1,  1,  0)
  
  my_matrix[17,] = c(-1,  0, -1,  0)
  my_matrix[18,] = c(-1,  0,  1,  0)
  my_matrix[19,] = c( 1,  0, -1,  0)
  my_matrix[20,] = c( 1,  0,  1,  0)
  
  my_matrix[21,] = c( 0, -1,  0, -1)
  my_matrix[22,] = c( 0, -1,  0,  1)
  my_matrix[23,] = c( 0,  1,  0, -1)
  my_matrix[24,] = c( 0,  1,  0,  1)
  
  return(my_matrix)
}