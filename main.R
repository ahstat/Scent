library(rgl)
rm(list = ls())
setwd("~/Documents/GitHub/scent")
debug = FALSE
source("helpers/sample_Sn.R")
source("helpers/measure_Sn.R")
source("helpers/measure_tangent_Sn.R")
source("helpers/move.R")
source("helpers/plots_Sn.R")
source("helpers/control.R")
source("helpers/tests.R")

# test1(seed = 1, t_max = "line")
# test1(seed = 1, t_max = "segment")
# test1(seed = 1, t_max = "semisegment")
# test2()
# test4()
# test5(seed = 0)
# test5(seed = 1)
# test6(TRUE)
# test6(FALSE)
# test7()
# test8("sphere")
# test8("circle")


square_on_sphere = function() {
  n_elem = 6
  dim_S = 2
  seed = 1
  my_matrix = sample_surface_sphere(n_elem, dim_S, seed = seed)
  my_matrix[1,] = c(-1,  0,  0)
  my_matrix[2,] = c( 1,  0,  0)
  my_matrix[3,] = c( 0, -1,  0)
  my_matrix[4,] = c( 0,  1,  0)
  my_matrix[5,] = c( 0,  0, -1)
  my_matrix[6,] = c( 0,  0,  1)
  return(my_matrix)
}

my_matrix = square_on_sphere()
n_elem = nrow(my_matrix)
N = 1000

## All combination of types and densitypes selected
vectypes = combin(n_elem)
for(i in 1:nrow(vectypes)) {
    evol_and_plot(my_matrix, i, vectypes, N, savepng = TRUE, plot_evol = FALSE)
}

# TODO: longitude/latitude.
