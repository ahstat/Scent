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
source("helpers/plots_S1.R")
source("helpers/plots.R")

rspin = function(n_elem) {2*rbinom(n_elem, 1, prob = 1/2)-1}
set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) ); seed = 100000000*rnorm(1)

# test1(seed = 1, t_max = "line")
# test1(seed = 1, t_max = "segment")
# test1(seed = 1, t_max = "semisegment")
# test2()
# test4()
# test5(seed = 0)
# test5(seed = 1)
# test6(TRUE)
# test6(FALSE)





  n_elem = 4
  dim_S = 2
  
  
  my_matrix = sample_surface_sphere(n_elem, dim_S, seed = seed)
  
  A = c( sqrt(8/9), 0, -1/3 )
  B = c( -sqrt(2/9), sqrt(2/3), -1/3 )
  C = c( -sqrt(2/9), -sqrt(2/3), -1/3 )
  D = c( 0, 0, 1 )
  my_matrix[1,] = A
  my_matrix[2,] = B
  my_matrix[3,] = C
  my_matrix[4,] = D
  
  # my_matrix[1,] = c(0, 1, 0)
  # my_matrix[2,] = c(0, 0, 1)
  # my_matrix[3,] = normalize_me(c(0.8, 0.8, 0))
  #types = rep(+1, nrow(my_matrix)) # whether ascent and descent on the global mixture function
  types = rspin(n_elem)# c(1, -1, -1, 1) # c(1, -1, -1,-1)#
  #densitypes = rep(+1, nrow(my_matrix)) # whether common and anti density
  densitypes = c(1, 1, 1,1)#rspin(n_elem) #c(-1, 1, 1, 1)
  alpha = 0.1
  N = 20
  
  Evolution = get_evol(my_matrix, N, Df, densitypes, types, alpha)
  distEvolution = dist_evol(Evolution)
  plot_dist_evol(distEvolution)
  plot_evolution(Evolution)
  

  # All seems to converge :(
  
  plot(distEvolution[2,3,], type = "l")
  
  

  

  
  


  
  
  






seed = 1
n_elem = 4
dim_S = 2
my_matrix = sample_surface_sphere(n_elem, dim_S, seed = seed)

A = c( sqrt(8/9), 0, -1/3 )
B = c( -sqrt(2/9), sqrt(2/3), -1/3 )
C = c( -sqrt(2/9), -sqrt(2/3), -1/3 )
D = c( 0, 0, 1 )
my_matrix[1,] = A
my_matrix[2,] = B
my_matrix[3,] = C
my_matrix[4,] = D

for(i1 in c(-1, 1)) {
  for(i2 in c(-1, 1)) {
    for(i3 in c(-1, 1)) {
      for(i4 in c(-1, 1)) {
        for(i5 in c(-1, 1)) {
          for(i6 in c(-1, 1)) {
            for(i7 in c(-1, 1)) {
              for(i8 in c(-1, 1)) {
                
                types = c(i1, i2, i3, i4)
                densitypes = c(i5, i6, i7 , i8)
                alpha = 0.1
                
                N = 1000
                Evolution = get_evol(my_matrix, N, Df, densitypes, types, alpha)
                
                distEvolution = dist_evol(Evolution)
                
                plot_dist_evol(distEvolution, 
                               main_title = paste0("(", i1, ",", i2, ",", i3,",", i4,",", i5,",", i6,",", i7,",", i8, ")"))
                
                
                
              }
            }
          }
        }
      }
    }
  }
}











































































































































dim_S = 1

sample_unif_circle = function(n_elem) {
  vec = (1:n_elem)-1
  vecExp = exp(1i*vec*2*pi/n_elem)
  my_matrix = cbind(Re(vecExp), Im(vecExp))
  return(my_matrix)
}

combin_types = function(n_elem, unitype = c(-1, 1)) {
  out = expand.grid(rep(list(unitype), n_elem))
  return(out)
}


n_elem = 2


my_matrix = sample_unif_circle(n_elem)



alpha = 0.1
N = 2000 #N = 20

vectypes = combin_types(2*nrow(my_matrix))
colnames(vectypes) = apply(expand.grid(c("type", "densitype"), 1:nrow(my_matrix)), 1, paste, collapse = "")
#vectypes

for(i in 1:nrow(vectypes)){
  types = as.numeric(vectypes[i,c(paste0("type", 1:nrow(my_matrix)))])
  densitypes = as.numeric(vectypes[i,c(paste0("densitype", 1:nrow(my_matrix)))])
  Evolution = get_evol(my_matrix, N, Df, densitypes, types, alpha)
  distEvolution = dist_evol(Evolution)
  print(paste0(i, "/", nrow(vectypes)))
  plot_dist_evol(distEvolution,
                main_title = paste(vectypes[i,], collapse = " "))
  
  main = i #paste0(names(vectypes[i,]), ": ", vectypes[i,], " / ", collapse = "")
  #png(paste0("plots/", i, ".png"))
  plot_evolution(Evolution, step_min = 1700, step_max = 1900, main = main)
  #dev.off()
}

tt = 1
plot_evolution(Evolution, step_min = tt, step_max = tt+10, main = main); tt = tt+1000



source("misc/misc_S2_to_latlong.R")

my_matrix_converted = t(apply(my_matrix, 1, latlong_func))


**..
