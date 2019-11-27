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

############
# Get norm #
############
norm_Eucl_vec = function(x) {
  # Return ||x|| of a point of R^n
  # https://stackoverflow.com/questions/10933945/
  sqrt(crossprod(x))
}

## Normalize a point of R^n
normalize_me = function(my_matrix) {
  # Normalize each row of a matrix 
  # The matrix consists of 1 row = 1 element in R^n
  #            and finally 1 row = 1 element in R^n belonging to S^{n-1}
  my_matrix / apply(my_matrix, 1, norm_Eucl_vec)
}
rm(normalize_me)

#############################################
# Distance between two points of the sphere #
#############################################
great_circle_distance = function(A, B) {
  # Points are elements of R^n belonging to S^{n-1}
  # https://en.wikipedia.org/wiki/N-vector
  acos(c(crossprod(A, B)))
}

############################################################################
# Rotation at constant speed on the great circle from A to B on the sphere #
############################################################################
rotated = function(A, B, t) {
  # A and B are points of R^n belonging to S^{n-1}
  # The function outputs a point of R^n belonging to S^{n-1} which is the
  # rotation from A in the direction B on a distance of t
  # A: Initial point
  # B: Final point after t = pi/2
  # So we should have:
  # * rotated(A, B, 0) gives A
  # * rotated(A, B, pi/2) gives B_prim (such that <A|B_prim>=0 and A,B,B_prim on the same circle)
  # * rotated(A, B, pi) gives -A
  # * rotated(A, B, 3*pi/2) gives -B_prim
  Lambda = c(crossprod(A, B))
  if(Lambda == 1) {
    stop("Points A and B are identical")
  }
  if(Lambda == -1) {
    stop("Points A and B are on the opposite each other")
  }
  B_prim = (B - Lambda * A) / sqrt(1 - Lambda^2)
  if(great_circle_distance(B, B_prim) > pi/2) {
    stop("The first solution is not OK")
    # Take the other solution if B_prim is distant from B
    # B_prim = -B_prim will work in this case
  }
  return(cos(t) * A + sin(t) * B_prim)
  # Explanation:
  # We suppose A and B are not a pole each other
  # Step 1: Find B' on the great circle A <--> B such that <A|B'>=0 [there are 2 B' like this]
  # Find B' of the form: B' = cos(t0) A + sin(t0) B for a certain t0
  # 0 = <A|B'> = <A|cos(t0) A + sin(t0) B> = cos(t0) + sin(t0) <A|B>
  # - cos(t0)/sin(t0) = <A|B>
  # tan(t0) = -1/<A|B>
  # t_1 = atan(-1/<A|B>) + pi or t_2 = atan(-1/<A|B>)
  # t_1 = -atan(1/<A|B>) + pi or t_2 = -atan(1/<A|B>)
  # t_1 = -pi/2 + atan(<A|B>) or t_2 = pi/2 + atan(<A|B>)
  # So B_prim_1 = cos(t_1) * A + sin(t_1) * B and B_prim_2 = -B_prim_1
  #    B_prim_1 = (<A|B> * A - B) / sqrt(1 + <A|B>^2)
  # And to normalize it is: sqrt((1-<A|B>^2)/(1+<A|B>^2))
  # So B_prim_1 = (<A|B> * A - B) / sqrt(1 - <A|B>^2) and B_prim_2 = -B_prim_1
  #c(crossprod(A, B_prim_1)) # ok == 0
  #c(crossprod(B_prim_1, B_prim_1)) # ok == 1
}

my_matrix = sample_surface_sphere(n_elem = 2, dim_S = 2, seed = 1234)
A = my_matrix[1,]
B = my_matrix[2,]
theta = seq(from = 0, to = 2*pi, length.out = 100)
line_from_A_to_A_by_B = t(sapply(theta, function(t) {rotated(A, B, t)}))
if(!all(round(apply(line_from_A_to_A_by_B, 1, norm_Eucl_vec), 10) == 1)) {
  stop("The output of rotated() is not a point of the unit sphere")
}
if(!all(round(rotated(A, B, 0) - A, 10) == 0)) {
  stop("rotated with t=0 does not give the original point")
}
if(!all(round(rotated(A, B, pi) - (-A), 10) == 0)) {
  stop("rotated with t=pi does not give the opposite point")
}
rm(my_matrix, A, B, theta, line_from_A_to_A_by_B)
# plot(t(apply(line_from_A_to_B, 1, latlong_func)) * 360 / (2*pi), xlim = c(-30, 30))

###################
# Plotting on S^2 #
###################
plot_sphere = function() {
  # https://stackoverflow.com/questions/34539268
  spheres3d(0, 0, 0, lit=FALSE, color="white")
  spheres3d(0, 0, 0, radius=1.01, lit=FALSE, color="black", front="lines")
}

plot_path_on_sphere = function(traj) {
  x <- traj[,1]
  y <- traj[,2]
  z <- traj[,3]
  spheres3d(x, y, z,col="black",radius=0.02)
}

plot_point_on_sphere = function(A, col = "red") {
  spheres3d(A[1], A[2], A[3], col = col, radius = 0.1)
}

if(debug) {
  my_matrix = sample_surface_sphere(n_elem = 2, dim_S = 2, seed = 1234)
  A = my_matrix[1,]
  B = my_matrix[2,]
  t_max = great_circle_distance(A, B) / 2  # t_max = 2*pi
  theta = seq(from = 0, to = t_max, length.out = 100)
  line_from_A_to_B = t(sapply(theta, function(t) {rotated(A, B, t)}))
  plot_sphere()
  plot_path_on_sphere(line_from_A_to_B)
  plot_point_on_sphere(A, "red")
  plot_point_on_sphere(B, "blue")
  rm(my_matrix, A, B, t_max, theta, line_from_A_to_B)
}

#######################################################################
# Can help to understand but not used in main code and only for n = 3 #
#######################################################################
latlong_func = function(xyz) {
  # Convert from cartesian to long/lat (geographic system)
  # https://en.wikipedia.org/wiki/N-vector
  x = xyz[1]
  y = xyz[2]
  z = xyz[3]
  lat = atan2(z, sqrt(x^2 + y^2))
  long = atan2(y, x)
  return(c(lat, long))
}

xyz_func = function(latlong) {
  # Convert from long/lat to cartesian (geographic system)
  # https://en.wikipedia.org/wiki/N-vector
  lat = latlong[1]
  long = latlong[2]
  x = cos(lat) * cos(long)
  y = cos(lat) * sin(long)
  z = sin(lat)
  return(c(x, y, z))
}

my_matrix = sample_surface_sphere(n_elem = 2, dim_S = 2, seed = 1234)
my_matrix_converted = t(apply(my_matrix, 1, latlong_func))
my_matrix_converted_converted = t(apply(my_matrix_converted, 1, xyz_func))
if(sum(round(my_matrix_converted_converted - my_matrix, 10)) != 0) {
  stop("Converting a point to coordinates and going back does not give original point")
}
rm(my_matrix, my_matrix_converted, my_matrix_converted_converted)

# xyz_func_gen = function(latlong) {
#   # Convert from long/lat to cartesian for dimension > 3 (geographic system)
#   # Not tested.
#   # https://fr.wikipedia.org/wiki/Coordonn%C3%A9es_sph%C3%A9riques#G%C3%A9n%C3%A9ralisation_en_dimension_n
#   # generalize geographic coordinates
#   # lat1, lat2, lat3 ... lat(n-1) long
#   sin_latlong = c(sin(latlong), 1)
#   prod_cos_latlong = c(1, cumprod(cos(latlong)))
#   x = rev(prod_cos_latlong * sin_latlong)
#   return(x)
# }
rm(latlong_func, xyz_func)