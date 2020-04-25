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
# plots.R / test-plots.R
# multiplots.R / test-multiplots.R

## Ongoing:
# todo.R

## Todo:
# document + pictures
# todo inside the text



# FIRST ----

# # Untitled 1 ----
#
# my_matrix = unif_on_S1(3)
# g = g_sin
# #densitypes = c(1, 1, -exp(1i * pi/4))
# densitypes = c(1, 1, -1)
# types = c(1, -1, -1)
# manifold = "E"
#
# ## Check convergence when alpha --> 0
# alpha = 1
# Tmax = 100
# N = Tmax / alpha
# Evolution = get_evol(N = N, my_matrix, g, densitypes, types, alpha, manifold)
# t1 = seq(from = 0, to = Tmax, by = alpha)
# t1 = t1[-length(t1)]
# velocity1 = velocity_func(Evolution, manifold, alpha)
# acceleration1 = acceleration_func(Evolution, manifold, alpha)
#
# Evolution = Evolution[,,1:3]
#
# alpha = 0.1
# Evolution = get_evol(N = Tmax/alpha, my_matrix, g, densitypes, types, alpha, manifold)
# t01 = seq(from = 0, to = Tmax, by = alpha)
# t01 = t01[-length(t01)]
# velocity01 = velocity_func(Evolution, manifold, alpha)
# acceleration01 = acceleration_func(Evolution, manifold, alpha)
#
# alpha = 0.01
# Evolution = get_evol(N = Tmax/alpha, my_matrix, g, densitypes, types, alpha, manifold)
# t001 = seq(from = 0, to = Tmax, by = alpha)
# t001 = t001[-length(t001)]
# velocity001 = velocity_func(Evolution, manifold, alpha)
# acceleration001 = acceleration_func(Evolution, manifold, alpha)
#
# plot(t1, velocity1[,1], type = "l")
# lines(t01, velocity01[,1], type = "l")
# lines(t001, velocity001[,1], type = "l")
#
# plot(t1, acceleration1[,1], type = "l")
# lines(t01, acceleration01[,1], type = "l")
# lines(t001, acceleration001[,1], type = "l")
#
# plot(t001, velocity001[,1], type = "l")
#
# plot(velocity1[,1], acceleration1[,1], type = "l")
# lines(velocity01[,1], acceleration01[,1], type = "l")
# lines(velocity001[,1], acceleration001[,1], type = "l")
#
# plot(velocity001[,1], acceleration001[,1], type = "l")
# range = 490:10000
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 1:490
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 490:650
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 650:3000
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 3000:10000
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# plot(velocity001[range,1], type = "l")
# plot(acceleration001[range,1], type = "l")
#
# plot(velocity001[,1], acceleration001[,1], type = "l", ylim = c(-0.18, 0.03), xlim = c(0, 0.6), asp = 1)
# lines(velocity001[,2], acceleration001[,2], type = "l", col = "red")
# lines(velocity001[,3], acceleration001[,3], type = "l", col = "blue")
#
# range = 3000:10000
# plot(t(Evolution[1,,range]), type = "l", asp = 1, ylim = c(-1.8, 1))
# lines(t(Evolution[2,,range]), asp = 1, type = "l", col = 2)
# lines(t(Evolution[3,,range]), asp = 1, type = "l", col = 3)
#
# plot(t(Evolution[1,,range]), type = "l", asp = 1)
#
#
# ## Untitled 2 ----
#
# my_matrix = unif_on_S1(3)
# g = g_sin
# densitypes = c(1, 1, -1)
# types = c(1, -1, -1)
# manifold = "S"
#
# ## Check convergence when alpha --> 0
# Tmax = 100
# alpha = 0.01
# Evolution = get_evol(N = Tmax/alpha, my_matrix, g, densitypes, types, alpha, manifold)
# t001 = seq(from = 0, to = Tmax, by = alpha)
# t001 = t001[-length(t001)]
# velocity001 = velocity_func(Evolution, manifold, alpha)
# acceleration001 = acceleration_func(Evolution, manifold, alpha)
#
# plot(t001, velocity001[,1], type = "l")
#
# plot(velocity001[,1], acceleration001[,1], type = "l")
# range = 490:10000
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 1:490
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 490:650
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 650:3000
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 3000:10000
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# plot(velocity001[range,1], type = "l")
# plot(acceleration001[range,1], type = "l")
#
# plot(velocity001[,1], acceleration001[,1], type = "l", ylim = c(-0.18, 0.03), xlim = c(0, 0.6), asp = 1)
# lines(velocity001[,2], acceleration001[,2], type = "l", col = "red")
# lines(velocity001[,3], acceleration001[,3], type = "l", col = "blue")
#
# range = 3000:10000
# plot(t(Evolution[1,,range]), type = "l", asp = 1, ylim = c(-1.8, 1))
# lines(t(Evolution[2,,range]), asp = 1, type = "l", col = 2)
# lines(t(Evolution[3,,range]), asp = 1, type = "l", col = 3)
#
# plot(t(Evolution[1,,range]), type = "l", asp = 1)
#
#
#
#
# range = 499:503
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
#
# range2 = 497:503
# plot(t(Evolution[1,,range2]), col = 1)
# text(t(Evolution[1,,range2]), labels = range2, cex = 0.7, pos = 3)
#
#
# ###################################################################################
#
#
# Tmax = 10
# alpha = 0.001
# Evolution = get_evol(N = Tmax/alpha, my_matrix, g, densitypes, types, alpha, manifold)
# t001 = seq(from = 0, to = Tmax, by = alpha)
# t001 = t001[-length(t001)]
# velocity001 = velocity_func(Evolution, manifold, alpha)
# acceleration001 = acceleration_func(Evolution, manifold, alpha)
#
# plot(t001, velocity001[,1], type = "l")
#
# plot(velocity001[,1], acceleration001[,1], type = "l")
# range = 4900:10000
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 1:4900
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 4900:6500
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# range = 6500:10000
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
#
# plot(velocity001[,1], acceleration001[,1], type = "l", ylim = c(-0.18, 0.03), xlim = c(0, 0.6), asp = 1)
# lines(velocity001[,2], acceleration001[,2], type = "l", col = "red")
# lines(velocity001[,3], acceleration001[,3], type = "l", col = "blue")
#
# range = 4960:5030
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
#
# range2 = 4970:5030
# plot(t(Evolution[1,,range2]), col = 1)
# text(t(Evolution[1,,range2]), labels = range2, cex = 0.7, pos = 3)
#
# # Redo:
# my_matrix = Evolution[,,4960]
# Tmax = 1
# alpha = 0.0001
# Evolution2 = get_evol(N = Tmax/alpha, my_matrix, g, densitypes, types, alpha, manifold)
# t001 = seq(from = 0, to = Tmax, by = alpha)
# t001 = t001[-length(t001)]
# velocity001 = velocity_func(Evolution2, manifold, alpha)
# acceleration001 = acceleration_func(Evolution2, manifold, alpha)
# range2 = 190:500
# plot(t(Evolution2[1,,range2]), col = 1, type = "p")
# text(t(Evolution2[1,,range2]), labels = range2, cex = 0.7, pos = 3)
#
# range = 1:10000
# plot(velocity001[range,1], acceleration001[range,1], type = "l")
# plot(velocity001[range2,1], acceleration001[range2,1], type = "l")
# plot(velocity001[range2,1], type = "l")
#
# plot(t(Evolution2[1,,range]), col = 1, type = "l", xlim = c(-1,1), ylim = c(-1,1))
# lines(t(Evolution2[2,,range]), col = 1)
# lines(t(Evolution2[3,,range]), col = 1)
#
# # Here is the behavior: velocity goes to 0 and rebound (because on the circle)
# # so normal to get somehow infinite acceleration due to numerical errors
# plot(velocity001[range2,1], type = "l")
#
#
#

# FOLLOWING ----




# my_matrix = unif_on_S1(5)
# g = g_sin
# #densitypes = list(c(1, 1, 1, 1, 1),
# #                  c(1, -1, 1, 1, 1))
# densitypes = c(1, 1, 1, 1, 1)
# types = combin(5, 2)# c(-1, -1, 1, 1, 1)
# #types = c(-1, -1, 1, 1, 1)
# manifold = "E"
# N = c(10, 100, 1000)
# Tmax = 3
# alpha = NULL
#
# order_grid = order_grid_default_func()
#
# config_for_plot = config_for_plot_func(plotting_option = 2,
#                                        t_labels = FALSE,
#                                        kind_of_palette = "default")
#
#
# my_experiments = define_experiments(my_matrix, g, densitypes, types, manifold,
#                                     N, alpha, Tmax,
#                                     order_grid)
# summary_list = compute_summary_list(my_experiments)
#
# summary_list_filtered = filter_summary_list(summary_list,
#                                             "velocity", "at the end is",
#                                             greater_than = 0.3, "for at least one particle")
#
# s1000 = summary_list[which(sapply(summary_list, function(x){x$N}) == 1000)]
# s10 = summary_list[which(sapply(summary_list, function(x){x$N}) == 10)]
# plot(sapply(s1000, function(x){max(x$velocity[1000,])}), sapply(s10, function(x){max(x$velocity[10,])}))
#
# my_pos_xy = data.frame(pos_x = c("time", "time"),
#                        pos_y = c("velocity", "acceleration"),
#                        stringsAsFactors = FALSE)
#
# my_experiments$title = paste0("types = (", sapply(my_experiments$types,
#                                                   function(x) {paste(x, collapse = ", ")}), ")",
#                               " / N = ", my_experiments$N)
#
# p_list = multiplot_scent(my_experiments, my_pos_xy, summary_list_filtered, config_for_plot)
# ggsave_func(p_list, outfile = "multipage.pdf")


## TODO: Add more choices for g the derivative of the density function
## Define g
# g_current = function(angle) {
#   -sin(3*angle)
# }
#
# g_current = function(angle) {
#   if(abs(angle) < pi/2) {
#     -sin(2*angle)
#   } else {
#     0
#   }
# }
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
# }
#
# deriv_dnorm = function(x, dim = 1) {
#   # parametre de forme.
#   out = - ((1 / (2*pi)^(1/2)) * exp(-sum(x^2)/2) * x) / C_deriv_dnorm(dim)
#   return(out)
# }
#
# C_deriv_dnorm = function(dim = 1) {
#   return((2*pi)^((dim-1)/2))
# }
#
# g_current = function(x) {deriv_dnorm(x, dim = 2)}
#
# sinc = function(x) {
#   ifelse(x == 0, 1, sin(x)/x)
# }
#
# x = seq(from = -pi, to = pi, length.out = 100)
# plot(x, sinc(x))
# lines(x, (cos(x) + 1) / 2, col = "blue")

## END define g


## TODO:
# * check why speed > 0.5 --> understood because good configuration: many points at distance close to pi/2
# * See simplest case where velocity=0 and acceleration change of sign


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
