library("fields") 
rm(list = ls())
setwd("~/Documents/GitHub/scent/")
source("helpers/distance.R")
source("helpers/ddensity.R")
source("helpers/helpers.R")
source("helpers/init.R")
source("helpers/plot.R")

##############
# Parameters #
##############
d = d_eucl
f = f_approxderivdnorm # f_derivdnorm
eps = 0.4
particles = init_4_sinmove(a=0.5, b=1.9)

########
# Code #
########
#particles$types = -particles$types
a = move(particles, 50, eps)

# Problem.... because not as before. Check derivative and behavior 
# (by comparing with previous version)
df = convert_array_to_df(a$positions)
df$alpha = (df$iteration - 1)/max(df$iteration) # for the moving effect
plotting(df, axes = TRUE)

##############
# Parameters #
##############
d = d_eucl
f = f_derivdnorm2
eps = 0.4
particles = init_4_sinmove(a=0.1, b=2.1)

# To prove: The normalization constant only slow down the complete behavior
# but does not change the trajectory.

########
# Code #
########
#particles$types = -particles$types
a = move(particles, 150, eps)

# Problem.... because not as before. Check derivative and behavior 
# (by comparing with previous version)
df = convert_array_to_df(a$positions)
df$alpha = (df$iteration - 1)/max(df$iteration) # for the moving effect
plotting(df, axes = TRUE)

###############################################################################

# points = matrix(runif(1000*100), nrow=1000, ncol=100)
# set.seed(1234)
# particles1 = matrix(runif(100*2, -3, 3), nrow = 100, ncol = 2)
# types1 = sign(runif(nrow(particles1), -1, 1))

# masse negative (page wiki)
# sinc function (page wiki)
# wrapped distribution (page wiki)
# noyau de la chaleur (page wiki)
