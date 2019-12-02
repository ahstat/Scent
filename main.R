library(rgl)
rm(list = ls())
setwd("~/Documents/GitHub/scent")
debug = FALSE
source("helpers/sample_Sn.R")
source("helpers/measure_Sn.R")
source("helpers/measure_tangent_Sn.R")
source("helpers/plots_S2.R")
source("helpers/tests.R")
source("helpers/move.R")

# test1(seed = 1, t_max = "line")
# test1(seed = 1, t_max = "segment")
# test1(seed = 1, t_max = "semisegment")
# test2()
# test3(seed = 1)


seed = 1
my_matrix = sample_surface_sphere(n_elem = 3, dim_S = 2, seed = seed)
#types = rep(+1, nrow(my_matrix)) # whether acscent and descent on the global mixture function
types = c(+1, -1, -1)
density_types = rep(+1, nrow(my_matrix)) # -1 will give you negative density for this particle
# density_types = c(-1,1,1)

## Change points
#A = c(0, 1, 0)
#B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
#C = c(0.5, 0.9, 0); C = C / sqrt(sum(C^2))
A = c(0, 1, 0)
B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
C = c(1, 0, 0)
my_matrix[1,] = A
my_matrix[2,] = B
my_matrix[3,] = C
## End custom changes

i = 1

my_matrix_list = list()
my_matrix_list[[1]] = my_matrix
for(k in 2:100) {
  my_matrix_pushed = push(my_matrix_list[[k-1]], Df, densitypes, types, alpha = 0.33) 
  my_matrix_list[[k]] = my_matrix_pushed
}

plot_sphere()
for(k in 1:100) {
  plot_all_points(i, my_matrix_list[[k]])
}

####### need to check, then continue




plot_all_points(i, my_matrix)
plot_all_points(i, my_matrix_pushed)

#plot_all_paths_from_i(i, my_matrix, my_matrix_deriv)
#plot_all_prim_from_i(i, my_matrix_deriv)
plot_all_tangent_from_i_new(i, my_matrix, M_logS_weighted/nrow(my_matrix))

for(i in 1:nrow(my_matrix)) {
  plot_mean_tangent_with_weights_from_i_new(i, my_matrix, M_logS_weighted)
}


plot_point_on_sphere(G, "goldenrod", radius)



apply(M_logS_weighted[i,,], 1, mean)


plot_mean_tangent_with_weights_from_i(i, density_types, my_matrix_derivdist, my_matrix_deriv, my_matrix)

mean_tangent = get_mean_tangent_with_weights_from_i(i, density_types, my_matrix_derivdist, my_matrix_deriv)
t = sqrt(sum((mean_tangent)^2))
G = rotated_from_derivative(my_matrix[i,], normalize_me(mean_tangent), t)
great_circle_distance(G, my_matrix[i,])
plot_point_on_sphere(G, "goldenrod", radius)

