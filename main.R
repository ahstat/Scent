rm(list = ls())
setwd("~/Documents/GitHub/scent/")
library(mvtnorm)
library(dplyr)
library(abind)
#library(tidyr)
#library(ggplot2)
# library(profvis); profvis({})
source("helpers/1_density.R")
source("helpers/2_dynamic.R")
source("helpers/3_plot.R")




position1 = c(0.25)
position2 = c(0.75)

bounds = c(1)
library(mvtnorm)
#############################################################

printbounds_func = function(bounds) {
  printbounds = sapply(bounds, function(b) {
    if(is.infinite(b)) {return("(-Inf, +Inf)")} else {return(paste0("[0, ", b, ")"))}
  })
  print(paste0("Particles will evolve in a ", length(bounds), "D space, with bounds: ", paste(printbounds, collapse = " x ")))
}



#############################################################
# Distance between positions on a torus with Euclidian norm #
#############################################################

## Representation of each position inside \prod_i [0, bound_i]
reduce_into_canonical_repr = function(position, bounds) {
  position0 = position
  for(i in 1:length(bounds)) {
    if(!is.infinite(bounds[i])) {
      position0[i] = position[i] %% bounds[i]
    } else {
      position0[i] = position[i]
    }
  }
  return(position0)
}

## Difference of positions inside \prod_i [0, bound_i]
diff_of_positions = function(position_i, position_j) {
  position_j - position_i
}

## Complementary difference of position for each axis i
complementary_abs_diff = function(abs_diff, bounds) {
  border_minus_abs_diff = rep(NA, length(abs_diff))
  for(i in 1:length(bounds)) {
    if(!is.infinite(bounds[i])) {
      border_minus_abs_diff[i] = bounds[i] - abs_diff[i]
    } else {
      border_minus_abs_diff[i] = abs_diff[i]
    }
  }
  return(border_minus_abs_diff)
}

##
# Function
##
dist_between_positions.torus = function(position1, position2,
                                        bounds = c(1, 2), where_evaluate) {
  
  FROM position 1
  TO position 2 and all replicas, from closer distant to most distant. TODO
  
  # Based on https://stackoverflow.com/questions/2123947
  reduce_into_canonical_repr(position, bounds)
    
  positions0 = reduce_into_canonical_repr(positions, bounds)
  global_diff = diff_of_positions(positions0)
  
  abs_diff = abs(global_diff)
  border_minus_abs_diff = complementary_abs_diff(abs_diff, bounds)
  closer_positions = pmin(abs_diff, border_minus_abs_diff)
  
  #d_positions = apply(closer_positions, c(1,2), function(x){sqrt(sum(x^2))})
  d_positions_list = list()
  for(k in 1:nrow(where_evaluate)) {
    # https://stackoverflow.com/questions/24520720/
    closer_positions_k = sweep(closer_positions, 3, as.numeric(where_evaluate[k,]), FUN = "+")
    d_positions_list[[k]] = apply(closer_positions_k, c(1,2), function(x){sqrt(sum(x^2))})
  }
  
  return(d_positions_list)
}


positions.
For each row of position, get position + where_evaluate

Given a position, 
1. take other positions
2. for each row (of other positions), we have a position+where_evaluate set of positions
3. order wrt distance to other positions (long but OK at first) # gives min dist --> max dist, good to check as function of sum_elem
4. get normal w.r.t to all of those positions (sum for replicas of one other position; then mean for other positions)
