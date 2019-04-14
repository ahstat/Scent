rm(list = ls())
library(mvtnorm)
library(abind)
library(tidyr)
library(ggplot2)
source("helpers/1_density.R")
source("helpers/2_dynamic.R")
source("helpers/3_plot.R")

particles = list(c(0,0,0), c(0,0,1), c(1,0,0), c(2,0,1))
types = list(c(1,1,-1), c(1,1,1), c(-1,-1,1), c(1,1,-1))
n = 1000
alpha = 1
bound = +Inf
sum_elem = 3L
arrayout = push_n(particles, types, n, alpha, Df, bound, sum_elem)
plot(delta_n(arrayout), ylim = c(0, range(delta_n(arrayout))[2]))
plot(arrayout[1,1,])
plot(arrayout[3,2,])

############################
# Two-dimensional plotting #
############################
particles = list(c(0,0), c(0,1), c(1,0), c(2,1))
types = list(c(1,-1), c(1,1), c(-1,1), c(1,-1))
n = 1000
alpha = 1
bound = +Inf
sum_elem = 3L
arrayout = push_n(particles, types, n, alpha, Df, bound, sum_elem)
plot(delta_n(arrayout), ylim = c(0, range(delta_n(arrayout))[2]))

idx_particle = 1
plot(arrayout[1,idx_particle,], arrayout[2,idx_particle,])
lines(arrayout[1,2,], arrayout[2,2,])

convert_array_to_df = function(arrayout) {
  df = as.data.frame.table(arrayout, base = list(paste0("dim", 1:dim(arrayout)[1]),
                                                 paste0("part", 1:dim(arrayout)[2]),
                                                 as.character(1:dim(arrayout)[3])))
  df[,3] = as.numeric(df[,3])
  
  df1 <- spread(data = df, key = Var1, value = Freq)
  head(df1)
  names(df1) = c("particle", "iteration", "dim1", "dim2")
  
  return(df1)
}

df = convert_array_to_df(arrayout)

# En vue de dessus, trajectoire de chaque particule (tous les x pas)
ggplot(df, aes(x = dim1, y = dim2, colour = particle)) + geom_path()

############################
# One-dimensional plotting #
############################



