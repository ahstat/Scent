rm(list = ls())
setwd("~/Documents/GitHub/scent/")
library(mvtnorm)
library(abind)
library(tidyr)
library(ggplot2)
# library(profvis); profvis({})
source("helpers/1_density.R")
source("helpers/2_dynamic.R")
source("helpers/3_plot.R")

###############
# Fragrance 1 #
###############
set.seed(1234)
N = 200
a = 1
n = 50
alpha = 1
bound = +Inf
sum_elem = 0L
particles = lapply(1:N, function(x) {c(runif(1, -a, a), runif(1, -a, a))})
types = lapply(1:N, function(x) {c(2*rbinom(1, 1, 0.5) - 1, 2*rbinom(1, 1, 0.5) - 1)})
plotting(particles, types, n, alpha, Df, bound, sum_elem, no_axes = TRUE)



#####################################


set.seed(1234)
N = 30
a = 0.1
particles = lapply(1:N, function(x) {c(runif(1, -a, a), runif(1, -a, a))})
#types = lapply(1:N, function(x) {c(2*rbinom(1, 1, 0.5) - 1, 2*rbinom(1, 1, 0.5) - 1)})
types = lapply(1:N, function(x) {if(x %% 2 == 0) {c(1, 1)} else {c(-1, -1)}})

n = 2000
alpha = 1
bound = +Inf
sum_elem = 3L
arrayout = push_n(particles, types, n, alpha, Df, bound, sum_elem)
plot(delta_n(arrayout), ylim = c(0, range(delta_n(arrayout))[2]))
df = convert_array_to_df(arrayout)
df$alpha = df$iteration/max(df$iteration)



ggplot(df, aes(x = jitter(dim1), y = jitter(dim2), colour = particle, alpha = alpha)) +
  geom_path() +
  theme(legend.position="none")


