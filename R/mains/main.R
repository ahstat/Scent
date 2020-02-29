library(rgl)
library(data.table)
library(ggplot2)
rm(list = ls())
setwd("~/Documents/GitHub/scent")
source("helpers/measure_E.R")
source("helpers/measure_H.R")
source("helpers/measure_S.R")
source("helpers/measure.R")

Log_weighted_M
Exp_M
dist_M

source("helpers/move.R")
source("helpers/sample_S.R")

# add R of the sphere
# add ??? for the hyperbolic space

source("helpers/plots.R")
source("helpers/control.R")
source("helpers/tests.R")


debug = FALSE
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

## Define my_matrix
#my_matrix = tetrahedron_on_sphere()
# my_matrix = square_on_Sn(dim_S = 2)
# my_matrix = vingtquatrecell()
#my_matrix = unif_circle(n_elem = 10)
my_matrix = matrix(NA, nrow = 2, ncol = 3)
my_matrix[1,] = c(0,-1,0)
the_higher_the_closer = 4
my_matrix[2,] = normalize_me(c(0,-1,1/the_higher_the_closer))
plot_mymatrix(my_matrix)

## Define Df
# Df_current = function(angle) {
#   -sin(3*angle)
# }
Df_current = function(angle) {
  if(abs(angle) < pi/4) {
    -sin(4*angle)
  } else {
    0
  }
}#Df

## Other parameters
N = 1000
alpha = 0.1
n_elem = nrow(my_matrix)
vectypes = combin(n_elem) # if > 10, need to see how to remove some
#vectypes = combin(n_elem, types = "type")

## Evolution
velocities = list()
max_val =  nrow(vectypes)
for(i in 1:max_val) {
  print(paste0(i, "/", nrow(vectypes)))
  Evolution = evol(my_matrix, i, vectypes, N, Df_current, alpha)
  plot_evolution(Evolution, main = i)
  vel = approx_velocity(Evolution)
  velocities[[i]] = data.frame(expe = as.character(i), step = 2:length(vel), vel = vel[-1])
  # distEvolution = dist_evol(Evolution)
  # approx_velocity(distEvolution)
}
out = rbindlist(velocities)

my_gg = ggplot(out, aes(step, vel, group = expe, label = expe)) + 
  geom_line(alpha = 0.1) + 
  theme_bw() + xlab("Step") + ylab("Velocity")
ggsave("tetraDf1000.png", my_gg, height = 10, width = 10, scale = 1.3)
val_out = unlist(lapply(velocities, function(x){x$vel[N-1]}))

idx = which(val_out > 0.05)
vectypes[idx,]
for(i in idx) {
  print(paste0(i, "/", nrow(vectypes)))
  Evolution = evol(my_matrix, i, vectypes, N, Df_current, alpha)
  plot_evolution(Evolution, main = i, step_min = N-10, step_max = N)
}






# #saveRDS(velocities, "velo_octaedre.RDS")
velo = readRDS("velo_octaedre.RDS")
val_out = unlist(lapply(velo, function(x){x$vel[N-1]}))
unique(sort(val_out))

val1 = 8.130516e-02
val2 = 1.325181e-01
val3 = 1.761211e-01
val4 = 1.999907e-01
idx1 = which(val_out > (val1 - 0.01) & val_out < (val1 + 0.01))
idx2 = which(val_out > (val2 - 0.01) & val_out < (val2 + 0.01))
idx3 = which(val_out > (val3 - 0.01) & val_out < (val3 + 0.01))
idx4 = which(val_out > (val4 - 0.01) & val_out < (val4 + 0.01))
# 
# i = idx4[1]
# Evolution = evol(my_matrix, i, vectypes, N)
# plot_evolution(Evolution)
# 


velocities = list()
max_val = 30 #nrow(vectypes) # 30 #
for(i in 1:max_val) {
  print(paste0(i, "/", nrow(vectypes)))
  Evolution = evol(my_matrix, i, vectypes, N)
  vel = approx_velocity(Evolution)
  velocities[[i]] = data.frame(expe = as.character(i), step = 2:length(vel), vel = vel[-1])

  distEvolution = dist_evol(Evolution)
  approx_velocity(distEvolution)
}
# 
# 
# 
# sinc = function(x) {
#   ifelse(x == 0, 1, sin(x)/x)
# }
# 
# x = seq(from = -pi, to = pi, length.out = 100)
# plot(x, sinc(x) + 
#        sinc(x + 2*pi) + sinc(x - 2*pi) +
#        sinc(x + 4*pi) + sinc(x - 4*pi) +
#        sinc(x + 6*pi) + sinc(x - 6*pi) +
#        sinc(x + 8*pi) + sinc(x - 8*pi) +
#        sinc(x + 10*pi) + sinc(x - 10*pi)
#        
# )
# lines(x, (cos(x) + 1) / 2, col = "blue")
