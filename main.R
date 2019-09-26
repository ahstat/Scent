library("fields") 
rm(list = ls())
setwd("~/Documents/GitHub/scent/")
source("helpers/distance.R")
source("helpers/ddensity.R")
source("helpers/helpers.R")
source("helpers/init.R")

##############
# Parameters #
##############
d = d_eucl
f = f_derivdnorm
eps = 0.4
particles = init_4_sinmove()

########
# Code #
########
positions = particles$positions
types = particles$types



positions = matrix(c(0, 0,
                     1, 0), nrow = 2, ncol = 2, byrow = TRUE)
types = c(1, 1)



plot(positions, asp = 1)
positions = move1(positions, types, eps)$positions
print(positions)

lines(positions, asp = 1, col = "red", type = "p")
...

###############################################################################

# points = matrix(runif(1000*100), nrow=1000, ncol=100)
# set.seed(1234)
# particles1 = matrix(runif(100*2, -3, 3), nrow = 100, ncol = 2)
# types1 = sign(runif(nrow(particles1), -1, 1))

# masse negative (page wiki)
# sinc function (page wiki)
# wrapped distribution (page wiki)
# noyau de la chaleur (page wiki)
