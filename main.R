library(fields)
library(abind)
library(tidyr)
library(ggplot2)
library(RUnit)
rm(list = ls())
debug = TRUE
setwd("~/Documents/GitHub/scent/")
source("helpers/directions/real.R")
source("helpers/directions/torus.R")
source("helpers/plot/plot2d.R")
#todo:
source("helpers/directions/main_directions.R")


source("helpers/derivdensity/approxderivdnorm.R")
source("helpers/derivdensity/derivdnorm.R")
source("helpers/derivdensity/main_derivdensity.R")
source("helpers/derivdensity/misc.R")


source("helpers/helpers.R")
source("helpers/positions.R")
source("helpers/plot.R")
source("helpers/moves.R")


#TODO:
# Distance and normal direction (to output once, together) for:
# 1. Eucl on R^N
# 2. Eucl on torus.
# With unit tests and graphical proves...


########
# Demo #
########
move_4_particles_1()
move_4_particles_2()

##############
# Parameters #
##############

d = d_eucl
f = f_derivdnorm2
steps = 50
eps = 1

set.seed(1234)
N = 200
a = 5
positions = lapply(1:N, function(x) {c(runif(1, -a, a), runif(1, -a, a))})
positions = t(sapply(positions, c))
types = sapply(1:N, function(x) {c(2*rbinom(1, 1, 0.5) - 1)})
particles = list("positions" = positions, "types" = types)
#return(particles)

########
# Code #
########
steps = 100000
evolution = move(particles, steps, eps, d, f, 10, action)
df = convert_array_to_df(evolution$positions)
df$alpha = (df$iteration - 1)/max(df$iteration) # for the moving effect
plotting_df(df, axes = FALSE)

###############################################################################

# Todo: grid of size (20,20) with particles close to (10,10)

d = function(x,y) {return(d_torus(x,y,5))}
f = f_derivdnorm
steps = 50
eps = 0.1

# set.seed(1234)
# N = 4
# maxpos = 1
# positions = lapply(1:N, function(x) {c(runif(1, 0, maxpos), runif(1, 0, maxpos))})
# positions = t(sapply(positions, c))

a=0.5; b=2.5
positions = matrix(10 + c(-a/2, -b/2,
                           a/2, -b/2,
                          -a/2,  b/2,
                           a/2,  b/2
),
                   ncol = 2, byrow = TRUE)
types = c(1, -1, 1, -1)

# types = sapply(1:N, function(x) {c(2*rbinom(1, 1, 0.5) - 1)})
# # types = c(1, -1, 1) #rep(1, N)
# types = c(-1, -1, -1 ,1)
particles = list("positions" = positions, "types" = types)

steps = 1000
evolution = move(particles, steps, eps, d, f, NA, NA) #10, actionTorus)
evolution$grad[2,2,]
# BUGGY.

#df = convert_array_to_df(evolution$positions)
#df$alpha = (df$iteration - 1)/max(df$iteration) # for the moving effect
#plotting_df(df, axes = FALSE)

# This problem to solve without changing color...
# https://stackoverflow.com/questions/38505412/r-geom-path-lines-closing-sometimes-how-to-keep-them-open






# points = matrix(runif(1000*100), nrow=1000, ncol=100)
# set.seed(1234)
# particles1 = matrix(runif(100*2, -3, 3), nrow = 100, ncol = 2)
# types1 = sign(runif(nrow(particles1), -1, 1))

# masse negative (page wiki)
# sinc function (page wiki)
# wrapped distribution (page wiki)
# noyau de la chaleur (page wiki)
