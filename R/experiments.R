# # TODO
#
# evol = function(my_matrix, i, vectypes, N, g, alpha = 0.1) {
#   types = as.numeric(vectypes[i, c(paste0("type", 1:nrow(my_matrix)))])
#   densitypes = as.numeric(vectypes[i, c(paste0("densitype", 1:nrow(my_matrix)))])
#   Evolution = get_evol(N, my_matrix, g, densitypes, types, alpha)
#   return(Evolution)
# }
#
# evol_and_plot = function(my_matrix,
#                          i, vectypes, N, alpha = 0.1,
#                          plot_dist = TRUE, plot_evol = TRUE, plot_evol_proj = FALSE,
#                          savepng = FALSE) {
#   Evolution = evol(my_matrix, i, vectypes, N, g, alpha)
#   distEvolution = dist_evol(Evolution)
#
#   print(paste0(i, "/", nrow(vectypes)))
#
#   if(plot_dist) {
#     if(savepng) {
#       png(paste0("plots/", i, ".png"), width = 1500, height = 1500)
#       plot_dist_evol(distEvolution, main_title = paste0("(", paste(vectypes[i,], collapse = " "), ")"))
#       dev.off()
#     } else {
#       plot_dist_evol(distEvolution, main_title = paste0("(", paste(vectypes[i,], collapse = " "), ")"))
#     }
#   }
#
#   if(plot_evol) {
#     step_min = max(N/2, N - 100)
#     step_max = N
#
#     if(savepng) {
#       png(paste0("plots/", i, ".png"))
#       main = i #paste0(names(vectypes[i,]), ": ", vectypes[i,], " / ", collapse = "")
#       plot_evolution(Evolution, step_min = step_min, step_max = step_min, main = main)
#       dev.off()
#     } else {
#       main = i #paste0(names(vectypes[i,]), ": ", vectypes[i,], " / ", collapse = "")
#       plot_evolution(Evolution, step_min = step_min, step_max = step_min, main = main)
#     }
#   }
#
#   if(plot_evol_proj) {
#     print("Not done for now. It corresponds to long/lat with S^2 and a segment in S^1")
#     # tt = 1
#     # plot_evolution(Evolution, step_min = tt, step_max = tt+10, main = main); tt = tt+1000
#     # source("misc/misc_S2_to_latlong.R")
#     # my_matrix_converted = t(apply(my_matrix, 1, latlong_func))
#   }
# }
#
# plot_dist_evol = function(distEvolution, main_title = "") {
#   n_elem = dim(distEvolution)[1]
#   par(mfrow=c(n_elem, n_elem), oma = c(5,4,0,0) + 1.5, mar = c(0,0,1,1) + 0.5)
#   for(i in 1:nrow(distEvolution)) {
#     for(j in 1:nrow(distEvolution)) {
#       #print(paste(i, j))
#       if(!all(is.na(distEvolution[i,j,]))) {
#         plot(distEvolution[i,j,],
#              type = "l",
#              #xaxt='n',
#              main = paste0("(", i, ",", j, ")"),
#              ylim = c(0-1e-8, pi+1e-8))
#       } else {
#         plot.new()
#       }
#     }
#   }
#   title(main_title, outer = TRUE)
#   par(mfrow=c(1,1))
# }

