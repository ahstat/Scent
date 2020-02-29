#
#
#
#
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
