#   http://r-pkgs.had.co.nz/
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E' == devtools::check()
#   Test Package:              'Ctrl + Shift + T'

## Done:
# measure.R / test-measure.R
# sample.R / test-sample.R
# move.R / test-move.R
# preplot.R / test-preplot.R
# grad_func.R (can add after)
# test-visual.R (can add after) visual_test_i() with i from 1 to 5
# control.R / test-control.R

## Ongoing:
# plots.R

## Todo:
# experiments.R
# todo_main.R
# todo_tests.R
# todo.R

# TODO after: adding weight (N1 particles at the same points vs N2 at other point)

# TODO after:
# 1. plots.R with preplots.R, then integrate test7 and test8 / grad_func.R /
# test-visual <--> todo_tests
# todo_main

# TODO: total displacement (=sum of velocity) What is the maximum of total displacement per particle? --> 1/2
# cf:
# 1 displacement for 2 particles --> 1/2 per particle
# 0.5 0.5 0.5 0.5 for 2 for 4 particles --> 1/2 per particle
# 0.75 0.25 0.25 0.25 --> 1.5
# 1 0 ... 0 --> 1

# TODO Close formula for -sin(theta) with 2 points going to speed 0.5 to 0 (formula of the whole function? looks like exponential decay) (formula for E and S looks the same!)


#####
# 1 #
#####
# Also check in all cases that g(0) = 0 somewhere
# create testg == testg somewhere

#####
# 2 #
#####
# ### Blabla misc
# theta = seq(from = -pi, to = pi, length.out = 100)
# plot(theta, tan(theta), type = "p")
# plot(theta, 1/(cos(theta)^2), type = "l")
# # https://fr.wikipedia.org/wiki/Harmonique_sphérique

#####
# 3 #
#####
# plot_all_prim_from_i = function(i, my_matrix_deriv, radius = 0.05) {
#   for(j in (1:nrow(my_matrix_deriv))[-i]) {
#     plot_point_on_sphere(my_matrix_deriv[i,j,], "lightgray", radius)
#   }
# }
#
# plot_all_tangent_from_i = function(i, my_matrix, my_matrix_deriv, weighted = FALSE,
#                                    dir_points, my_matrix_derivdist) {
#   N = nrow(my_matrix)
#   if(!weighted) {
#     W = rep(1, N)
#   } else {
#     W = dir_points*my_matrix_derivdist[i,] / N
#   }
#   t_vec = seq(from = 0.1, to = 1, by = 0.05)
#   for(j in (1:nrow(my_matrix))[-i]) {
#     traj_tangent = t(sapply(t_vec, function(t) {my_matrix[i,] + t * W[j] * my_matrix_deriv[i,j,]}))
#     plot_path_on_sphere(traj_tangent, "lightgray")
#   }
# }
#
# plot_all_tangent_from_i_new = function(i, my_matrix, M_logS_weighted) {
#   N = nrow(my_matrix)
#   t_vec = seq(from = 0.1, to = 1, by = 0.05)
#   for(j in (1:nrow(my_matrix))[-i]) {
#     traj_tangent = t(sapply(t_vec, function(t) {my_matrix[i,] + t * M_logS_weighted[i,j,]}))
#     plot_path_on_sphere(traj_tangent, "orange")
#   }
# }
#
# plot_mean_tangent_with_weights_from_i = function(i, dir_points, my_matrix_derivdist, my_matrix_deriv, my_matrix) {
#   mean_tangent = get_mean_tangent_with_weights_from_i(i, dir_points, my_matrix_derivdist, my_matrix_deriv)
#   t_vec = seq(from = 0.1, to = 1, by = 0.05)
#   traj_tangent = t(sapply(t_vec, function(t) {my_matrix[i,] + t * mean_tangent}))
#   plot_path_on_sphere(traj_tangent, "goldenrod")
# }
#
# plot_mean_tangent_with_weights_from_i_new = function(i, my_matrix, M_logS_weighted) {
#   mean_tangent = apply(M_logS_weighted[i,,], 2, mean)
#   t_vec = seq(from = 0, to = 1, by = 0.05)
#   traj_tangent = t(sapply(t_vec, function(t) {my_matrix[i,] + t * mean_tangent}))
#   plot_path_on_sphere(traj_tangent, "goldenrod")
# }

#####
# 4 #
#####
# ###################
# # S2 to lat long: #
# ###################
# ## Can help to understand but not used in main code and only for n = 3
# latlong_func = function(xyz) {
#   # Convert from cartesian to long/lat (geographic system)
#   # https://en.wikipedia.org/wiki/N-vector
#   x = xyz[1]
#   y = xyz[2]
#   z = xyz[3]
#   lat = atan2(z, sqrt(x^2 + y^2))
#   long = atan2(y, x)
#   return(c(lat, long))
# }
#
# xyz_func = function(latlong) {
#   # Convert from long/lat to cartesian (geographic system)
#   # https://en.wikipedia.org/wiki/N-vector
#   lat = latlong[1]
#   long = latlong[2]
#   x = cos(lat) * cos(long)
#   y = cos(lat) * sin(long)
#   z = sin(lat)
#   return(c(x, y, z))
# }
#
# my_matrix = sample_on_S(n_elem = 2, dim_S = 2, seed = 1234)
# my_matrix_converted = t(apply(my_matrix, 1, latlong_func))
# my_matrix_converted_converted = t(apply(my_matrix_converted, 1, xyz_func))
# if(sum(round(my_matrix_converted_converted - my_matrix, 10)) != 0) {
#   stop("Converting a point to coordinates and going back does not give original point")
# }
# rm(my_matrix, my_matrix_converted, my_matrix_converted_converted)
#
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


