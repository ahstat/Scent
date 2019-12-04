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
  seed = 1234
  
  my_matrix = sample_surface_sphere(n_elem = 4, dim_S = 2, seed = seed)
  #types = rep(+1, nrow(my_matrix)) 
  types = rep(+1, nrow(my_matrix)) # whether ascent and descent on the global mixture function
  densitypes = rep(+1, nrow(my_matrix)) # whether common and anti density
  alpha = 1
  
  ## Change points
  # A = c(0, 1, 0)
  # B = normalize_me(c(0, 0.9, 0.5))
  # C = c(1, 0, 0)
  A = c(0, 1, 0)
  B = c(0, 0, 1)
  C = c(1, 0, 0)
  
  A = c( sqrt(8/9), 0, -1/3 )
  B = c( -sqrt(2/9), sqrt(2/3), -1/3 )
  C = c( -sqrt(2/9), -sqrt(2/3), -1/3 )
  D = c( 0, 0, 1 ) 
  
  my_matrix[1,] = A
  my_matrix[2,] = B
  my_matrix[3,] = C
  my_matrix[4,] = D
  A = my_matrix[1,]
  B = my_matrix[2,]
  C = my_matrix[3,]
  
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
  
  densitypes = c(1, 1, 1, -1)
  #densitypes = c(1, 1, -1)
  M2 = matrix2_of_weighted_contribution_with_densitypes(M1, densitypes)
  Bprim_onA = M2[1, 2,]
  Cprim_onA = M2[1, 3,]
  Aprim_onB = M2[2, 1,]
  Cprim_onB = M2[2, 3,]
  Aprim_onC = M2[3, 1,]
  Bprim_onC = M2[3, 2,]
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)  
  plot_segment_R_n(A, A + Bprim_onA, col = colB)
  plot_segment_R_n(A, A + Cprim_onA, col = colC)
  plot_segment_R_n(B, B + Aprim_onB, col = colA)
  plot_segment_R_n(B, B + Cprim_onB, col = colC)
  plot_segment_R_n(C, C + Aprim_onC, col = colA)
  plot_segment_R_n(C, C + Bprim_onC, col = colB)
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  
  M3 = matrix3_of_mean_action(M2)
  Bprim_onA = M2[1, 2,]
  Cprim_onA = M2[1, 3,]
  Aprim_onB = M2[2, 1,]
  Cprim_onB = M2[2, 3,]
  Aprim_onC = M2[3, 1,]
  Bprim_onC = M2[3, 2,]
  mean_onA = M3[1,]
  mean_onB = M3[2,]
  mean_onC = M3[3,]
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)
  plot_segment_R_n(A, A + Bprim_onA, col = colB)
  plot_segment_R_n(A, A + Cprim_onA, col = colC)
  plot_segment_R_n(B, B + Aprim_onB, col = colA)
  plot_segment_R_n(B, B + Cprim_onB, col = colC)
  plot_segment_R_n(C, C + Aprim_onC, col = colA)
  plot_segment_R_n(C, C + Bprim_onC, col = colB)
  plot_segment_R_n(A, A + mean_onA, col = "goldenrod")
  plot_segment_R_n(B, B + mean_onB, col = "goldenrod")
  plot_segment_R_n(C, C + mean_onC, col = "goldenrod")
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  
  #types = c(1,1,-1)
  types = c(-1,-1,-1, -1)
  M4 = matrix4_of_mean_action_with_types(M3, types, alpha)
  Bprim_onA = M2[1, 2,]
  Cprim_onA = M2[1, 3,]
  Aprim_onB = M2[2, 1,]
  Cprim_onB = M2[2, 3,]
  Aprim_onC = M2[3, 1,]
  Bprim_onC = M2[3, 2,]
  mean_onA = M4[1,]
  mean_onB = M4[2,]
  mean_onC = M4[3,]
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)
  plot_segment_R_n(A, A + Bprim_onA, col = colB)
  plot_segment_R_n(A, A + Cprim_onA, col = colC)
  plot_segment_R_n(B, B + Aprim_onB, col = colA)
  plot_segment_R_n(B, B + Cprim_onB, col = colC)
  plot_segment_R_n(C, C + Aprim_onC, col = colA)
  plot_segment_R_n(C, C + Bprim_onC, col = colB)
  plot_segment_R_n(A, A + mean_onA, col = "darkgoldenrod")
  plot_segment_R_n(B, B + mean_onB, col = "darkgoldenrod")
  plot_segment_R_n(C, C + mean_onC, col = "darkgoldenrod")
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  
  my_matrix_pushed = matrix5_of_mean_actions_with_types_on_sphere(my_matrix, M4)
  mean_onA = M4[1,]
  mean_onB = M4[2,]
  mean_onC = M4[3,]
  mean_onA_sphere = my_matrix_pushed[1,]
  mean_onB_sphere = my_matrix_pushed[2,]
  mean_onC_sphere = my_matrix_pushed[3,]
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)
  plot_segment_R_n(A, A + mean_onA, col = "red")
  plot_segment_R_n(B, B + mean_onB, col = "blue")
  plot_segment_R_n(C, C + mean_onC, col = "green")
  plot_segment_S_n(A, mean_onA_sphere, col = "darkred")
  plot_segment_S_n(B, mean_onB_sphere, col = "darkblue")
  plot_segment_S_n(C, mean_onC_sphere, col = "darkgreen")
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  plot_point(mean_onA_sphere, "darkred")
  plot_point(mean_onB_sphere, "darkblue")
  plot_point(mean_onC_sphere, "darkgreen")
  
}

test5()











seed = 1

my_matrix = sample_surface_sphere(n_elem = 3, dim_S = 1, seed = seed)
#types = rep(+1, nrow(my_matrix)) 
types = rep(+1, nrow(my_matrix)) # whether ascent and descent on the global mixture function
densitypes = rep(+1, nrow(my_matrix)) # whether common and anti density
alpha = 1

## Change points
A = c(0, 1)
B = normalize_me(c(0.1, 0.9))
C = c(1, 0)
my_matrix[1,] = A
my_matrix[2,] = B
my_matrix[3,] = C
## End custom changes
colA = "red"
colB = "blue"
colC = "green"

plot_circle()
plot_point_on_circle(A, colA)
plot_point_on_circle(B, colB)
plot_point_on_circle(C, colC)

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

plot_segment_R_2(A, A + Bprim_onA, col = colB)
plot_segment_R_2(A, A + Cprim_onA, col = colC)
plot_segment_R_2(B, B + Aprim_onB, col = colA)
plot_segment_R_2(B, B + Cprim_onB, col = colC)
plot_segment_R_2(C, C + Aprim_onC, col = colA)
plot_segment_R_2(C, C + Bprim_onC, col = colB)
plot_segment_S_n(A, B)
plot_segment_S_n(A, C)
plot_segment_S_n(B, C)