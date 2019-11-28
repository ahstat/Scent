library(rgl)
rm(list = ls())
setwd("~/Documents/GitHub/scent")
debug = FALSE#TRUE
source("helpersSphere/helpers.R")

my_matrix = sample_surface_sphere(n_elem = 100, dim_S = 10, seed = 1)
A = my_matrix[1,]
B = my_matrix[2,]
t_max = 2*pi #great_circle_distance(A, B) #/ 2  # t_max = 2*pi
theta = seq(from = 0, to = t_max, length.out = 100)
line_from_A_to_B = t(sapply(theta, function(t) {rotated(A, B, t)}))
apply(line_from_A_to_B, 1, norm_Eucl_vec)
plot(theta, apply(line_from_A_to_B, 1, function(x){great_circle_distance(line_from_A_to_B[1,], x)}))

plot_sphere()
plot_path_on_sphere(line_from_A_to_B)
plot_point_on_sphere(A, "red")
plot_point_on_sphere(B, "blue")


###


plot(theta, tan(theta), type = "p")
plot(theta, 1/(cos(theta)^2), type = "l")

# https://fr.wikipedia.org/wiki/Harmonique_sphérique