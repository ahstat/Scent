# S2

##########################################################################
# Sample uniform distribution on the surface of a unit sphere (20180501) #
##########################################################################
# http://mathworld.wolfram.com/HyperspherePointPicking.html
# Pick n_elem points on S^{n-1}
sample_surface_sphere = function(n_elem, dim_S = 2, seed = 1234) {
  set.seed(seed)
  n = dim_S + 1
  M = matrix(rnorm(n * n_elem), ncol = n)
  sample = M / sqrt(apply(M^2, 1, sum))
  return(sample)
}

norm_Eucl_vec = function(x) {
  # https://stackoverflow.com/questions/10933945/
  sqrt(crossprod(x))
}

normalize_me = function(my_matrix) {
  my_matrix / apply(my_matrix, 1, norm_Eucl_vec)
}

latlong_func = function(xyz) {
  # https://en.wikipedia.org/wiki/N-vector
  x = xyz[1]
  y = xyz[2]
  z = xyz[3]
  lat = atan2(z, sqrt(x^2 + y^2))
  long = atan2(y, x)
  return(c(lat, long))
}

xyz_func = function(latlong) {
  # https://en.wikipedia.org/wiki/N-vector
  lat = latlong[1]
  long = latlong[2]
  x = cos(lat) * cos(long)
  y = cos(lat) * sin(long)
  z = sin(lat)
  return(c(x, y, z))
}

xyz_func_gen = function(latlong) {
  # generalize geographic coordinates
  # lat1, lat2, lat3 ... lat(n-1) long
  sin_latlong = c(sin(latlong), 1)
  prod_cos_latlong = c(1, cumprod(cos(latlong)))
  x = rev(prod_cos_latlong * sin_latlong)
  return(x)
}

xyz_func_alt = function(latlong, r = 1) {
  # https://fr.wikipedia.org/wiki/Coordonn%C3%A9es_sph%C3%A9riques#G%C3%A9n%C3%A9ralisation_en_dimension_n
  # TODO:
  sin_theta = sin(latlong)
  theta1_to_n_minus_2 = latlong[-length(latlong)] # latitudes in [0, pi] each
  theta_n_minus_1 = latlong[length(latlong)] # longitude in [0, 2*pi]
  x1 = cos(theta1)
  x2 = sin(theta1) * cos(theta2)
  xi = sin(theta1) * sin(theta2) * cos(latlong[i])
  ...
  x_n_minus_1 = sin(theta1) * ... * sin(theta_n_minus_2) * cos(theta_n_minus_1)
  x_n = sin(theta1) * ... * sin(theta_n_minus_2) * sin(theta_n_minus_1)
}




great_circle_distance = function(xyz_a, xyz_b) {
  # https://en.wikipedia.org/wiki/N-vector
  acos(c(crossprod(xyz_a, xyz_b)))
}

n_elem = 2
my_matrix = sample_surface_sphere(n_elem, seed = 1)


my_matrix_converted = t(apply(my_matrix, 1, latlong_func))
my_matrix_converted_converted = t(apply(my_matrix_converted, 1, xyz_func))

crossprod(my_matrix)
great_circle_distance(my_matrix[1,], my_matrix[2,])

round(my_matrix_converted_converted - my_matrix, 10)


t = seq(from = 0, to = 1, length.out = 100)
cont_line = t(sapply(t, function(t) {(1-t) * my_matrix[1,] + t * my_matrix[2,]}))
cont_line = normalize_me(cont_line)

cont_line2 = t(sapply(t, function(t) {(1-t) * (-my_matrix[1,]) + t * my_matrix[2,]}))
cont_line2 = normalize_me(cont_line2)

cont_line3 = t(sapply(t, function(t) {(1-t) * (-my_matrix[1,]) + t * (-my_matrix[2,])}))
cont_line3 = normalize_me(cont_line3)

cont_line4 = t(sapply(t, function(t) {(1-t) * my_matrix[1,] + t * (-my_matrix[2,])}))
cont_line4 = normalize_me(cont_line4)

plot(t(apply(cont_line, 1, latlong_func)) * 360 / (2*pi), xlim = c(-30, 30))
lines(t(apply(cont_line2, 1, latlong_func)) * 360 / (2*pi), col = "blue")
lines(t(apply(cont_line3, 1, latlong_func)) * 360 / (2*pi), col = "red")
lines(t(apply(cont_line4, 1, latlong_func)) * 360 / (2*pi), col = "yellow")

# derivative in t = 1 (action of 1 on 2):
# a = my_matrix[2,] - my_matrix[1,]; normalize_me(t(matrix(a)))

library(rgl)

# https://stackoverflow.com/questions/34539268
x <- cont_line[,1]
y <- cont_line[,2]
z <- cont_line[,3]
spheres3d(0,0,0,lit=FALSE,color="white")
spheres3d(0,0,0,radius=1.01,lit=FALSE,color="black",front="lines")
spheres3d(x, y, z,col="black",radius=0.02)

x <- cont_line2[,1]
y <- cont_line2[,2]
z <- cont_line2[,3]
spheres3d(x, y, z,col="blue",radius=0.02)

x <- cont_line3[,1]
y <- cont_line3[,2]
z <- cont_line3[,3]
spheres3d(x, y, z,col="red",radius=0.02)

x <- cont_line4[,1]
y <- cont_line4[,2]
z <- cont_line4[,3]
spheres3d(x, y, z,col="yellow",radius=0.02)

