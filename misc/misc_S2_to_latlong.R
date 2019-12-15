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
