rm(list = ls())
setwd("~/Documents/GitHub/scent/archives/")
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
df = compute(particles, types, n, alpha, Df, bound, sum_elem)
plotting(df, axes = FALSE)

############
# Test may #
############
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

#################
# Fragrance May #
#################
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
pdf("out_may.pdf", 2, 2)
plotting(df, axes = TRUE) + theme_bw() + theme(legend.position="none",
                                               axis.title.x=element_blank(),
                                               axis.text.x=element_blank(),
                                               axis.ticks.x=element_blank(),
                                               axis.title.y=element_blank(),
                                               axis.text.y=element_blank(),
                                               axis.ticks.y=element_blank())
dev.off()
