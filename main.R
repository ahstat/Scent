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

######
#
#####################################
set.seed(1234)
N = 2
a = 1
n = 1000
alpha = 1
bound = +Inf
sum_elem = 0L
particles = list(c(0, 0), c(1, 1), c(0, 11), c(1, 10))
types = list(c(1, 1), c(-1, -1), c(1, 1), c(-1, -1))
df = compute(particles, types, n, alpha, Df, bound, sum_elem)
plotting(df, axes = TRUE)

aa = df %>% filter(particle == "part1")


######
#
#####################################
set.seed(1234)
N = 2
a = 1
n = 1000
alpha = 1
bound = +10
sum_elem = 3L
particles = list(c(0, 0), c(1, 1), c(0, 5), c(1, 4))
types = list(c(1, 1), c(-1, -1), c(1, 1), c(-1, -1))
df = compute(particles, types, n, alpha, Df, bound, sum_elem)
plotting(df, axes = TRUE)

######
#
#####################################
set.seed(1234)
N = 2
a = 1
n = 10000
alpha = 1
bound = +8
sum_elem = 3L
particles = list(c(0, 0), c(1, 1), c(1, 4), c(0, 5))
types = list(c(1, 1), c(-1, -1), c(1, 1), c(-1, -1))
df = compute(particles, types, n, alpha, Df, bound, sum_elem)
plotting(df, axes = TRUE)

