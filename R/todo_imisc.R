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
