library(ggplot2)
library(dplyr)

theme_blank = function() {
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())
}

heads_alpha = 1 # full color
heads_shape = 19 # filled point

tails_alpha = 0.3
tails_shape = 1 # circled point
cols = c("partic1" = "red", "partic2" = "black")

trail = "solid"
trail = "fade" # fade from begining to end
trail = 6 # fade up to 6 steps back


plotting_option = 2


Tmax = 1
N = 20

df_plot1 = data.frame(t = seq(from = 0, to = Tmax, length.out = N),
                      pos_x = 1:N,
                      pos_y = 1:N)
df_plot2 = data.frame(t = seq(from = 0, to = Tmax, length.out = N),
                      pos_x = 1:N,
                      pos_y = rev(1:N))
df_plot_list = list()
df_plot_list[["partic1"]] = df_plot1
df_plot_list[["partic2"]] = df_plot2
# df_plot_list is the input / each element of the list must be ordered in time

Nsizes = sapply(df_plot_list, nrow)
if(!all(Nsizes == Nsizes[1])) {
  stop("All particles must have the same number of steps")
}



if(trail == "fade") {
  alpha_func = function(N) {
    (1:N)/N
  }
} else if(trail == "solid") {
  alpha_func = function(N) {
    1
  }
} else if(is.numeric(trail)) {
  base_trail = (1:trail)/trail
  alpha_func = function(N) {
    out = tail(base_trail, N)
    N0 = length(out)
    if(N > N0) {
      out = c(rep(0, N - N0), out)
    }
    return(out)
  }
} else {
  stop("trail must be 'fade', 'solid' or a positive integer")
}

df_plot = bind_rows(df_plot_list, .id = "id") %>%
  rename(points = id) %>%
  mutate(points = factor(points))
df_plot$alpha = as.vector(sapply(Nsizes, alpha_func))


df_tails = df_plot %>%
  group_by(points) %>%
  arrange(t) %>%
  summarise(t = t[1], pos_x = pos_x[1], pos_y = pos_y[1])
df_heads = df_plot %>%
  group_by(points) %>%
  arrange(-t) %>%
  summarise(t = t[1], pos_x = pos_x[1], pos_y = pos_y[1])


df_tails$alpha = tails_alpha
df_heads$alpha = heads_alpha



p = ggplot(df_plot, aes(x = pos_x, y = pos_y, color = points, alpha = alpha)) +
  geom_path() + scale_colour_manual(values = cols) +
  guides(alpha=FALSE)

p = p +
  geom_point(data = df_tails, shape = tails_shape) +
  geom_point(data = df_heads, shape = heads_shape)

if(plotting_option == 1) {
  p = p + theme_bw()
} else if(plotting_option == 2) {
  p = p + theme_bw() + theme(legend.position = "none")
} else if(plotting_option == 3) {
  p = p + theme_blank()
}

p

## to continue


# plot_evolution = function(Evolution, step_min = 1, step_max = NA, step_by = 1, ...) {
#   if(dim(Evolution)[2] == 2) {
#     plot_evolution_circle(Evolution, step_min, step_max, ...)
#   } else if(dim(Evolution)[2] == 3) {
#     plot_evolution_sphere(Evolution, step_min, step_max, step_by)
#   } else {
#     stop(paste0("Evolution should live on S1 or S2, but found: S", dim(Evolution)[2]-1))
#   }
# }
#



# plot_mymatrix = function(my_matrix, ...) {
#   N = 1
#   Evolution = array(NA, dim = c(dim(my_matrix), N))
#   Evolution[,,1] = my_matrix
#   plot_evolution(Evolution, step_min = 1, step_max = NA, ...)
# }
#
# ###################
# # Plotting on S^2 #
# ###################
# plot_evolution_sphere = function(Evolution, step_min, step_max, step_by) {
#   if(is.na(step_max)) {
#     step_max = dim(Evolution)[3]
#   }
#
#   plot_sphere()
#   for(step in seq(from = step_min, to = step_max, by = step_by)){
#     step = floor(step)
#     colpalette = c("red", "blue", "green", "yellow", "magenta", "cyan", "orange", "darkgray")
#     for(k in 1:nrow(Evolution)) {
#       if(k <= length(colpalette)) {
#         plot_point_on_sphere(Evolution[k,,step], colpalette[k])
#       } else {
#         plot_point_on_sphere(Evolution[k,,step], "black")
#       }
#     }
#   }
# }
#
# plot_sphere = function() {
#   # https://stackoverflow.com/questions/34539268
#   rgl::spheres3d(0, 0, 0, lit = FALSE, color = "white")
#   rgl::spheres3d(0, 0, 0, radius = 1.01, lit = FALSE, color = "black", front = "lines")
# }
#
# plot_point_on_sphere = function(A, col = "red", radius = 0.1) {
#   rgl::spheres3d(A[1], A[2], A[3], col = col, radius = radius)
# }
#
# plot_path_on_sphere = function(traj, col = "black", radius = 0.02) {
#   # traj is a general trajectory
#   x <- traj[,1]
#   y <- traj[,2]
#   z <- traj[,3]
#   rgl::spheres3d(x, y, z, col = col, radius = radius)
# }
#
# # if(debug) {
# #   my_matrix = sample_on_S(n_elem = 2, dim_S = 2, seed = 1234)
# #   A = my_matrix[1,]
# #   B = my_matrix[2,]
# #   t_max = .distance_S_great_circle(A, B) / 2  # t_max = 2*pi
# #   theta = seq(from = 0, to = t_max, length.out = 100)
# #   line_from_A_to_B = t(sapply(theta, function(t) {.rotated(A, B, t)}))
# #   plot_sphere()
# #   plot_path_on_sphere(line_from_A_to_B)
# #   plot_point_on_sphere(A, "red")
# #   plot_point_on_sphere(B, "blue")
# #   rm(my_matrix, A, B, t_max, theta, line_from_A_to_B)
# # }
#
# plot_all_points_on_sphere = function(i, my_matrix, radius = 0.05,
#                                      col_i = "red",
#                                      col_others = "black") {
#   plot_point_on_sphere(my_matrix[i,], col_i, radius)
#   for(j in (1:nrow(my_matrix))[-i]) {
#     plot_point_on_sphere(my_matrix[j,], col_others, radius)
#   }
# }
#
# segment_R_n_func = function(A, B) {
#   # on the tangent space in R^{n-1}. Not ok for segment in S^{n-1}
#   theta = seq(from = 0, to = 1, length.out = 100)
#   segment_R_n = t(sapply(theta, function(t) {t * A + (1-t) * B}))
#   return(segment_R_n)
# }
#
# segment_S_n_func = function(A, B, t_max = "segment") {
#   if(t_max == "line") {
#     t_max = 2*pi
#   } else if(t_max == "segment") {
#     t_max = .distance_S_great_circle(A, B)
#   } else if(t_max == "semisegment") {
#     t_max = .distance_S_great_circle(A, B) / 2
#   }
#   theta = seq(from = 0, to = t_max, length.out = 100)
#   line_from_A_to_B = t(sapply(theta, function(t) {.rotated(A, B, t)}))
#   return(line_from_A_to_B)
# }
#
# plot_segment_R_n = function(A, B, col = "black") {
#   segment_R_n = segment_R_n_func(A, B)
#   plot_path_on_sphere(segment_R_n, col = col)
# }
#
# plot_segment_S_n = function(A, B, col = "black") {
#   segment_S_n = segment_S_n_func(A, B)
#   plot_path_on_sphere(segment_S_n, col = col)
# }
#
# ###################
# # Plotting on S^1 #
# ###################
# plot_evolution_circle = function(Evolution, step_min, step_max, ...) {
#   if(is.na(step_max)) {
#     step_max = dim(Evolution)[3]
#   }
#
#   par(mfrow = c(1,1))
#   plot_circle(...)
#   for(step in seq(from = step_min, to = step_max)){
#     step = floor(step)
#     for(k in 1:nrow(Evolution)) {
#       plot_point_on_circle(Evolution[k,,step], col = k+1, 2)
#     }
#   }
# }
#
# plot_circle = function(...) {
#   # https://stackoverflow.com/questions/22265704/drawing-circle-in-r/22266006
#   # initialize a plot
#   plot(c(-1, 1), c(-1, 1), type = "n", asp = 1, ...)
#
#   # prepare "circle data"
#   radius <- 1
#   theta <- seq(0, 2 * pi, length = 200)
#
#   # draw the circle
#   lines(x = radius * cos(theta), y = radius * sin(theta))
# }
#
# plot_point_on_circle = function(A, col = "red", radius = 0.1) {
#   points(A[1], A[2], col = col, lwd = radius)
# }
#
# plot_path_on_circle = function(traj, col = "black", radius = 2) {
#   x <- traj[,1]
#   y <- traj[,2]
#   lines(x, y, col = col, lwd = radius)
# }
#
# plot_segment_R_2 = function(A, B, col = "black") {
#   segment_R_2 = segment_R_n_func(A, B)
#   plot_path_on_circle(segment_R_2, col = col)
# }
