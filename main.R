rm(list = ls())
setwd("~/Documents/GitHub/scent/")
library(mvtnorm)
library(abind)
library(tidyr)
library(ggplot2)
source("helpers/1_density.R")
source("helpers/2_dynamic.R")
source("helpers/3_plot.R")

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




# library(profvis)
# profvis({
particles = list(c(0,0), c(0,1), c(1,0), c(2,1))
types = list(c(1,-1), c(1,1), c(-1,1), c(1,-1))
n = 200
alpha = 1
bound = +5
sum_elem = 0L
arrayout = push_n(particles, types, n, alpha, Df, bound, sum_elem)
# })

plot(delta_n(arrayout), ylim = c(0, range(delta_n(arrayout))[2]))

df = convert_array_to_df(arrayout)
df$dim1 = df$dim1 %% bound
df$dim2 = df$dim2 %% bound
df$alpha = df$iteration/max(df$iteration)

# En vue de dessus, trajectoire de chaque particule (tous les x pas)
ggplot(df, aes(x = jitter(dim1), y = jitter(dim2), colour = particle, alpha = alpha)) + geom_path()

dd = df %>% filter(particle == "part4")

############################
# One-dimensional plotting #
############################

N = 200

set.seed(1234)
a = 1
particles = lapply(1:N, function(x) {c(runif(1, -a, a), runif(1, -a, a))})
types = lapply(1:N, function(x) {c(2*rbinom(1, 1, 0.5) - 1, 2*rbinom(1, 1, 0.5) - 1)})

# particles = list(c(1,0), c(-0.5,sqrt(3)/2), c(-0.5,-sqrt(3)/2))
# types = list(c(-1,1), c(1,1), c(1,1))
n = 50
alpha = 1
bound = +Inf
sum_elem = 3L
arrayout = push_n(particles, types, n, alpha, Df, bound, sum_elem)
plot(delta_n(arrayout), ylim = c(0, range(delta_n(arrayout))[2]))

df = convert_array_to_df(arrayout)
df$alpha = df$iteration/max(df$iteration)

# En vue de dessus, trajectoire de chaque particule (tous les x pas)
ggplot(df, aes(x = jitter(dim1), y = jitter(dim2), colour = particle, alpha = alpha)) + 
  geom_path() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())




#####################################



particles = list(c(1,0.0000001), c(2,0))
types = list(c(1,1), c(-1,-1))
n = 500
alpha = 1
bound = +Inf
sum_elem = 3L
arrayout = push_n(particles, types, n, alpha, Df, bound, sum_elem)
plot(delta_n(arrayout), ylim = c(0, range(delta_n(arrayout))[2]))

df = convert_array_to_df(arrayout)
df$alpha = df$iteration/max(df$iteration)

# En vue de dessus, trajectoire de chaque particule (tous les x pas)
ggplot(df, aes(x = jitter(dim1), y = jitter(dim2), colour = particle, alpha = alpha)) + geom_path()


library(dplyr)
x = (df %>% filter(particle == "part1"))$dim1
y = (df %>% filter(particle == "part2"))$dim1
plot(y-x)

# https://rdrr.io/cran/NPflow/man/mvnpdfC.html
# Higher density derivative for normal distribution in any dimension?
# In dim 1: exp(-1/2)*sqrt(1/(2*pi)) = 1/sqrt(2*pi*exp(1))
# In dim n: 
# https://github.com/mfasiolo/mvnfast
# https://www.gnu.org/software/gsl/manual/html_node/The-Multivariate-Gaussian-Distribution.html

1/(2*pi*e)^(N/2)

N/(2*pi*e)^N


N = 1:10
plot(N, N/(2*pi*exp(1))^N)
plot(N, 1/(N/(2*pi*exp(1))^N))


# retrieve equ diff










# library(profvis)
# profvis({

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

# })
#png("out100000.png", 1000, 1000)
ggplot(df, aes(x = jitter(dim1), y = jitter(dim2), colour = particle, alpha = alpha)) +
  geom_path() +
  theme(legend.position="none") #+
  #xlim(c(0,5)) + ylim(c(-10,0)) +
  # theme(axis.line=element_blank(),axis.text.x=element_blank(),
  #       axis.text.y=element_blank(),axis.ticks=element_blank(),
  #       axis.title.x=element_blank(),
  #       axis.title.y=element_blank(),legend.position="none",
  #       panel.background=element_blank(),panel.border=element_blank(),
  #       panel.grid.major=element_blank(),
  #       panel.grid.minor=element_blank(),plot.background=element_blank())
#dev.off()



# Set mixture locally, neglect if > threshold.