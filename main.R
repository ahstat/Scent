library(rgl)
rm(list = ls())
setwd("~/Documents/GitHub/scent")
debug = FALSE
source("helpers/sample_Sn.R")
source("helpers/measure_Sn.R")
source("helpers/measure_tangent_Sn.R")
source("helpers/tests.R")
source("helpers/move.R")
source("helpers/plots_S2.R")

# test1(seed = 1, t_max = "line")
# test1(seed = 1, t_max = "segment")
# test1(seed = 1, t_max = "semisegment")
# test2()
# test4()

test5 = function(seed = 1) {
  seed = 1
  
  my_matrix = sample_surface_sphere(n_elem = 3, dim_S = 2, seed = seed)
  #types = rep(+1, nrow(my_matrix)) 
  types = rep(+1, nrow(my_matrix)) # whether ascent and descent on the global mixture function
  densitypes = rep(+1, nrow(my_matrix)) # whether common and anti density
  alpha = 1
  
  ## Change points
  A = c(0, 1, 0)
  B = normalize_me(c(0, 0.9, 0.5))
  C = c(1, 0, 0)
  my_matrix[1,] = A
  my_matrix[2,] = B
  my_matrix[3,] = C
  ## End custom changes
  colA = "red"
  colB = "blue"
  colC = "green"
  
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)
  
  # row = point 0 of the tangent space
  # col = outside point as seen on the tangent
  M1 = matrix1_of_weighted_contribution(my_matrix, Df)
  Bprim_onA = M1[1, 2,]
  Cprim_onA = M1[1, 3,]
  Aprim_onB = M1[2, 1,]
  Cprim_onB = M1[2, 3,]
  Aprim_onC = M1[3, 1,]
  Bprim_onC = M1[3, 2,]
  
  great_circle_distance(A, C)
  great_circle_distance(B, C)
  
  plot_segment_R_n(A, A + Bprim_onA, col = colB)
  plot_segment_R_n(A, A + Cprim_onA, col = colC)
  plot_segment_R_n(B, B + Aprim_onB, col = colA)
  plot_segment_R_n(B, B + Cprim_onB, col = colC)
  plot_segment_R_n(C, C + Aprim_onC, col = colA)
  plot_segment_R_n(C, C + Bprim_onC, col = colB)
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  
  # readline(prompt="Press [enter] to continue")
  # plot_all_points(i, my_matrix)
  # readline(prompt="Press [enter] to continue")
  # plot_all_points(i, my_matrix_pushed)

  
  M2 = matrix2_of_weighted_contribution_with_densitypes(M1, densitypes)
  M3 = matrix3_of_mean_action(M2)
  M4 = matrix4_of_mean_action_with_types(M3, types, alpha)
  my_matrix_pushed = matrix5_of_mean_actions_with_types_on_sphere(my_matrix, M4)
  
}

test5()
