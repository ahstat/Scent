# library(rgl)
# library(data.table)
# library(ggplot2)
# library(dplyr)
# rm(list = ls())
# setwd("~/Documents/GitHub/scent")
# debug = FALSE
# source("helpers/measure_E.R")
# source("helpers/measure_H.R")
# source("helpers/measure_S.R")
# source("helpers/measure.R")
# source("helpers/sample_Sn.R")
# source("helpers/measure_Sn.R")
# source("helpers/measure_tangent_Sn.R")
# source("helpers/move.R")
# source("helpers/plots_Sn.R")
# source("helpers/control.R")
# source("helpers/tests.R")
#
# # test1(seed = 1, t_max = "line")
# # test1(seed = 1, t_max = "segment")
# # test1(seed = 1, t_max = "semisegment")
# # test2()
# # test4()
# # test5(seed = 0)
# # test5(seed = 1)
# # test6(TRUE)
# # test6(FALSE)
# # test7()
# # test8("sphere")
# # test8("circle")
#
# ## Define my_matrix
# #my_matrix = tetrahedron_on_sphere()
# # my_matrix = square_on_Sn(dim_S = 2)
# # my_matrix = vingtquatrecell()
# #my_matrix = unif_circle(n_elem = 10)
# #my_matrix = matrix(NA, nrow = 2, ncol = 3)
# my_matrix = matrix(NA, nrow = 4, ncol = 3)
# my_matrix[1,] = c(0,-1,0)
# # the_higher_the_closer = 4
# # my_matrix[2,] = normalize_me(c(0,-1,1/the_higher_the_closer))
# my_matrix[2,] = rotated(my_matrix[1,], normalize_me(c(0,-1,1)), pi/16)
#
# my_matrix[3,] = c(-1,0,0)
#
# #my_matrix[4,] = normalize_me(c(-1,0,1/the_higher_the_closer))
# my_matrix[4,] = rotated(my_matrix[3,], normalize_me(c(-1,0,1)), pi/16)
#
# plot_mymatrix(my_matrix)
#
# ## Define g
# # g_current = function(angle) {
# #   -sin(3*angle)
# # }
#
# g_current = function(angle) {
#   if(abs(angle) < pi/2) {
#     -sin(2*angle)
#   } else {
#     0
#   }
# }#g
#
# g_current = function(angle) {
#   if(abs(angle) < pi/4) {
#     -sin(4*angle)
#   } else {
#     0
#   }
# }
#
# g_current = function(angle) {
#   -sin(10*angle)
#   #-sin(2*angle)
#   # if(abs(angle) < pi/1.6) {
#   #   -sin(1.6*angle)
#   # } else {
#   #   0
#   # }
# }#g
#
# # deriv_dnorm = function(x, dim = 1) {
# #   # parametre de forme.
# #   out = - ((1 / (2*pi)^(1/2)) * exp(-sum(x^2)/2) * x) / C_deriv_dnorm(dim)
# #   return(out)
# # }
# #
# # C_deriv_dnorm = function(dim = 1) {
# #   return((2*pi)^((dim-1)/2))
# # }
# #
# # g_current = function(x) {deriv_dnorm(x, dim = 2)}
#
# ## Other parameters
# N = 50000
# alpha = 0.003
# # N = 1000
# # alpha = 0.1
# n_elem = nrow(my_matrix)
# vectypes = combin(n_elem) # if > 10, need to see how to remove some
# #vectypes = combin(n_elem, types = "type")
# vectypes = vectypes %>% filter(type1 == -1, densitype1 == 1, type2 == 1, densitype2 == 1)
#
# ## Evolution
# velocities = list()
# max_val =  nrow(vectypes)
# #vectypes[15,] # as euclidian: c(-1, 1, -1, 1), and densitype==1
# # other good is vectypes[2,]:  # c(-1, 1, 1, -1) and densitype = c(1,1,-1,-1)
# for(i in 1:max_val) {
#   #i = 15
#   print(paste0(i, "/", nrow(vectypes)))
#   Evolution = evol(my_matrix, i, vectypes, N, g_current, alpha)
#   vel = approx_velocity(Evolution)
#   velocities[[i]] = data.frame(expe = as.character(i), step = 2:length(vel), vel = vel[-1])
#   plot(vel)
#   #plot_evolution(Evolution, 1, 1000, 10, main = i)
#   #plot_evolution(Evolution, 1, 18000, 300, main = i)
#   plot_evolution(Evolution, 1, 5000, 10, main = i)
#   #plot_evolution(Evolution, 100, 100, main = i)
#   # distEvolution = dist_evol(Evolution)
#   # approx_velocity(distEvolution)
# }
# out = rbindlist(velocities)
#
# my_gg = ggplot(out, aes(step, vel, group = expe, label = expe)) +
#   geom_line(alpha = 0.1) +
#   theme_bw() + xlab("Step") + ylab("Velocity")
# ggsave("tetrag1000.png", my_gg, height = 10, width = 10, scale = 1.3)
# val_out = unlist(lapply(velocities, function(x){x$vel[N-1]}))
#
# idx = which(val_out > 0.05)
# vectypes[idx,]
# for(i in idx) {
#   print(paste0(i, "/", nrow(vectypes)))
#   Evolution = evol(my_matrix, i, vectypes, N, g_current, alpha)
#   plot_evolution(Evolution, main = i, step_min = N-10, step_max = N)
#   #plot_evolution(Evolution, 100, 100, main = i)
#   # distEvolution = dist_evol(Evolution)
#   # approx_velocity(distEvolution)
# }
#
# out = rbindlist(velocities)
#
# my_gg = ggplot(out, aes(step, vel, group = expe, label = expe)) +
#   geom_line(alpha = 0.1) +
#   theme_bw() + xlab("Step") + ylab("Velocity")
# ggsave("tetrag1000.png", my_gg, height = 10, width = 10, scale = 1.3)
# val_out = unlist(lapply(velocities, function(x){x$vel[N-1]}))
#
# idx = which(val_out > 0.05)
# vectypes[idx,]
# for(i in idx) {
#   print(paste0(i, "/", nrow(vectypes)))
#   Evolution = evol(my_matrix, i, vectypes, N, g_current, alpha)
#   plot_evolution(Evolution, main = i, step_min = N-10, step_max = N)
# }
#
# # #saveRDS(velocities, "velo_octaedre.RDS")
# velo = readRDS("velo_octaedre.RDS")
# val_out = unlist(lapply(velo, function(x){x$vel[N-1]}))
# unique(sort(val_out))
#
# val1 = 8.130516e-02
# val2 = 1.325181e-01
# val3 = 1.761211e-01
# val4 = 1.999907e-01
# idx1 = which(val_out > (val1 - 0.01) & val_out < (val1 + 0.01))
# idx2 = which(val_out > (val2 - 0.01) & val_out < (val2 + 0.01))
# idx3 = which(val_out > (val3 - 0.01) & val_out < (val3 + 0.01))
# idx4 = which(val_out > (val4 - 0.01) & val_out < (val4 + 0.01))
# #
# # i = idx4[1]
# # Evolution = evol(my_matrix, i, vectypes, N)
# # plot_evolution(Evolution)
# #
#
# velocities = list()
# max_val = 30 #nrow(vectypes) # 30 #
# for(i in 1:max_val) {
#   print(paste0(i, "/", nrow(vectypes)))
#   Evolution = evol(my_matrix, i, vectypes, N)
#   vel = approx_velocity(Evolution)
#   velocities[[i]] = data.frame(expe = as.character(i), step = 2:length(vel), vel = vel[-1])
#
#   distEvolution = dist_evol(Evolution)
#   approx_velocity(distEvolution)
# }
# #
# #
# #
# # sinc = function(x) {
# #   ifelse(x == 0, 1, sin(x)/x)
# # }
# #
# # x = seq(from = -pi, to = pi, length.out = 100)
# # plot(x, sinc(x) +
# #        sinc(x + 2*pi) + sinc(x - 2*pi) +
# #        sinc(x + 4*pi) + sinc(x - 4*pi) +
# #        sinc(x + 6*pi) + sinc(x - 6*pi) +
# #        sinc(x + 8*pi) + sinc(x - 8*pi) +
# #        sinc(x + 10*pi) + sinc(x - 10*pi)
# #
# # )
# # lines(x, (cos(x) + 1) / 2, col = "blue")
