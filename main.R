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
source("helpers/control.R")

# test1(seed = 1, t_max = "line")
# test1(seed = 1, t_max = "segment")
# test1(seed = 1, t_max = "semisegment")
# test2()
# test4()
# test5(seed = 0)
# test5(seed = 1)


test6 = function(seed = 0, types = c(1, 1, 1), densitypes = c(1, 1, 1), alpha = 1) {
  

  
  if(seed == 0) {
    ## Custom change 0
    A = c(0, 1, 0)
    B = normalize_me(c(0, 0.9, 0.5))
    C = c(1, 0, 0)
    my_matrix[1,] = A
    my_matrix[2,] = B
    my_matrix[3,] = C
  } else if(seed == 1) {
    ## Custom change 1
    A = c(0, 1, 0)
    B = c(0, 0, 1)
    C = c(1, 0, 0)
    my_matrix[1,] = A
    my_matrix[2,] = B
    my_matrix[3,] = C
  }
  
  A = my_matrix[1,]
  B = my_matrix[2,]
  C = my_matrix[3,]
  colA = "red"
  colB = "blue"
  colC = "green"
  
  my_matrix_pushed = push(my_matrix, Df, densitypes, types, alpha = 1)
  mean_onA_sphere = my_matrix_pushed[1,]
  mean_onB_sphere = my_matrix_pushed[2,]
  mean_onC_sphere = my_matrix_pushed[3,]
  plot_sphere()
  plot_point(A, colA)
  plot_point(B, colB)
  plot_point(C, colC)
  plot_segment_S_n(A, mean_onA_sphere, col = paste0("dark", colA))
  plot_segment_S_n(B, mean_onB_sphere, col = paste0("dark", colB))
  plot_segment_S_n(C, mean_onC_sphere, col = paste0("dark", colC))
  plot_segment_S_n(A, B)
  plot_segment_S_n(A, C)
  plot_segment_S_n(B, C)
  plot_point(mean_onA_sphere, paste0("dark", colA))
  plot_point(mean_onB_sphere, paste0("dark", colB))
  plot_point(mean_onC_sphere, paste0("dark", colC))
  
  seed = 1
  
  
  n_elem = 6
  dim_S = 1
  rspin = function(n_elem) {2*rbinom(n_elem, 1, prob = 1/2)-1}
  set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
  seed = 100000000*rnorm(1)
  my_matrix = sample_surface_sphere(n_elem, dim_S, seed = seed)
  #types = rep(+1, nrow(my_matrix)) # whether ascent and descent on the global mixture function
  types = rspin(n_elem)# c(1, -1, -1, 1)
  #densitypes = rep(+1, nrow(my_matrix)) # whether common and anti density
  densitypes = rspin(n_elem) #c(-1, 1, 1, 1)
  alpha = 0.1
  
  N = 600
  Evolution = array(NA, dim = c(dim(my_matrix), N))
  Evolution[,,1] = my_matrix
  for(i in 2:N) {
    Evolution[,,i] = push(Evolution[,,i-1], Df, densitypes, types, alpha)
  }
  
  distEvolution = array(NA, dim = c(n_elem, n_elem, N))
  for(i in 1:N) {
    distEvolution[,,i] = matrix_of_distances(Evolution[,,i])
  }
  
  par(mfrow=c(n_elem, n_elem), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.5)
  for(i in 1:nrow(distEvolution)) {
    for(j in 1:nrow(distEvolution)) {
      print(paste(i, j))
      if(!all(is.na(distEvolution[i,j,]))) {
        plot(distEvolution[i,j,], type = "l", xaxt='n', main = paste0("(", i, ",", j, ")"))
      } else {
        plot.new()
      }
    }
  }
  
  
  # All seems to converge :(
  
  plot(distEvolution[2,3,], type = "l")
  
  
  
  my_matrix = Evolution[,,1]
  plot_matrix(my_matrix)
  apply(my_matrix, 1, norm_Eucl_vec)
  
  plot_matrix = function(my_matrix) {
    plot_sphere()
    for(i in 1:nrow(my_matrix)) {
      plot_point(my_matrix[i,], "black")
    }
  }
  
  
}


# https://en.wikipedia.org/wiki/Thomson_problem

# http://www.etudes.ru/en/etudes/thomson-problem/
# In spaces of higher dimensions the problem is rigorously solved only occasionally.
# For example, for 120 charges in 4-dimensional space the solution is a regular 120-faced polyhedron.
# A curious result was received in 24-dimensional space. Minimum energy configuration for 196569 charges is provided by celebrated Leech lattice.

# http://www-wales.ch.cam.ac.uk/~wales/CCD/Thomson/table.html



# A = c( sqrt(8/9), 0, -1/3 )
# B = c( -sqrt(2/9), sqrt(2/3), -1/3 )
# C = c( -sqrt(2/9), -sqrt(2/3), -1/3 )
# D = c( 0, 0, 1 ) 


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