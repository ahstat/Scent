printbounds_func = function(bounds) {
  printbounds = sapply(bounds, function(b) {
    if(is.infinite(b)) {return("(-Inf, +Inf)")} else {return(paste0("[0, ", b, ")"))}
  })
  print(paste0("Particles will evolve in a ", length(bounds), "D space, with bounds: ", paste(printbounds, collapse = " x ")))
}

##############################
# Evaluation modulo `bounds` #
##############################
where_evaluate_func = function(bounds, sum_elem = 3L) {

  # The following helper function defines the set {z ; z mod bounds = x}
  
  # In context:
  # Let f be the normal density in dimension one.
  # If we specify the bound to 2 (corresponding to [-1,1] for example), 
  # evaluation of the density in x=0 corresponds to the sum:
  #    f(0) + f(2) + f(-2) + f(-4) + f(4) + ... = sum[f(z) ; z mod 2 = 0]
  
  ##
  # x: position for which the set is created, living in dimension `dim`
  # bounds: bounds length for each dim, as a vector of length `dim`
  # sum_elem: number of elements to consider on left and right
  ##
  
  ## Example:
  # where_evaluate_func(c(+Inf,2,3), sum_elem = 3L)
  my_seq = seq(from = -sum_elem, to = sum_elem, by = 1L)
  dim = length(bounds)
  # Source: helpers/input_array.R for triangle-pursuit code
  list_to_expand = list()
  for(i in 1:dim) {
    if(is.infinite(bounds[i])) {
      list_to_expand[[i]] = 0
    } else {
      list_to_expand[[i]] = my_seq
    }
  }
  xmod = expand.grid(list_to_expand)
  for(i in 1:dim) {
    if(is.infinite(bounds[i])) {
      xmod[,i] = xmod[,i]
    } else {
      xmod[,i] = bounds[i] * xmod[,i]
    }
  }
  return(xmod)
}

#############################################################
# Distance between positions on a torus with Euclidian norm #
#############################################################

## Representation of each position inside \prod_i [0, bound_i]
reduce_into_canonical_repr = function(positions, bounds) {
  dim_space = ncol(positions)
  positions0 = matrix(rep(NA, prod(dim(positions))), ncol = dim_space)
  for(i in 1:length(bounds)) {
    if(!is.infinite(bounds[i])) {
      positions0[,i] = positions[,i] %% bounds[i]
    } else {
      positions0[,i] = positions[,i]
    }
  }
  return(positions0)
}

## Difference of positions inside \prod_i [0, bound_i]
diff_of_positions = function(positions0) {
  dim_space = ncol(positions0)
  nb_part = nrow(positions0)
  global_diff = array(NA, dim = c(nb_part, nb_part, dim_space))
  for(i in 1:nb_part) {
    for(j in 1:nb_part) {
      global_diff[i,j,] = positions0[j,] - positions0[i,]
    }
  }
  return(global_diff)
}

## Complementary difference of position for each axis i
complementary_abs_diff = function(abs_diff, bounds) {
  border_minus_abs_diff = array(rep(NA, length(abs_diff)), dim = dim(abs_diff))
  for(i in 1:length(bounds)) {
    if(!is.infinite(bounds[i])) {
      border_minus_abs_diff[,,i] = bounds[i] - abs_diff[,,i]
    } else {
      border_minus_abs_diff[,,i] = abs_diff[,,i]
    }
  }
  return(border_minus_abs_diff)
}

##
# Function
##
dist_between_positions.torus = function(positions, bounds = c(1, 2), where_evaluate) {
  # Based on https://stackoverflow.com/questions/2123947
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

##
# Example
##
positions = matrix(c(0.9, 0.9,
                     0.1, 0.2,
                     0.0, 0.0,
                     9.1, -9.1),
                   ncol = 2, byrow = T)
bounds = c(1, 1)
sum_elem = 0L
where_evaluate = where_evaluate_func(bounds, sum_elem)
dist_between_positions.torus(positions, bounds, where_evaluate)[[1]]
# First, positions between [0,1]^2 are (0.9,0.9), (0.1,0.2), (0,0), (0.1,0.9)
# Then,
# For 1<->2, the smaller is d((-0.1,-0.1),(0.1,0.2)) i.e. sqrt(0.2^2+0.3^2)=0.36
# For 1<->3, the smaller is d((-0.1,-0.1),(0,0)) i.e. sqrt(0.1^2+0.1^2)=0.14
# For 1<->4, the smaller is d((-0.1,0.9),(0.1,0.9)) i.e. sqrt(0.2^2+0^2)=0.2
# For 2<->3, the smaller is d((0.1,0.2),(0,0)) i.e. sqrt(0.1^2+0.2^2)=0.22
# For 2<->4, the smaller is d((0.1,0.2),(0.1,-0.1)) i.e. sqrt(0^2+0.3^2)=0.3
# For 3<->4, the smaller is d((0,0),(0.1,-0.1)) i.e. sqrt(0.1^2+0.1^2)=0.14
rm(positions, bounds)

##
# Unit test
##
positions = matrix(c(0.9, 0.9,
                     0.1, 0.2,
                     0.0, 0.0,
                     9.1, -9.1),
                   ncol = 2, byrow = T)
bounds = c(1, 1)
sum_elem = 0L
where_evaluate = where_evaluate_func(bounds, sum_elem)
checkEqualsNumeric(dist_between_positions.torus(positions, bounds, where_evaluate)[[1]],
                   matrix(c(0, sqrt(0.2^2+0.3^2), sqrt(0.1^2+0.1^2), sqrt(0.2^2+0^2),
                            sqrt(0.2^2+0.3^2), 0, sqrt(0.1^2+0.2^2), sqrt(0^2+0.3^2),
                            sqrt(0.1^2+0.1^2), sqrt(0.1^2+0.2^2), 0, sqrt(0.1^2+0.1^2),
                            sqrt(0.2^2+0^2), sqrt(0^2+0.3^2), sqrt(0.1^2+0.1^2), 0),
                          ncol = 4, byrow = TRUE))
rm(positions, bounds)

#####################################################
# Normalized direction between positions on a torus #
#####################################################

##
# Helper
##
which.pmin = function(x, y) {
  pmin_out = pmin(x, y)
  which_x_is_min = which(abs(pmin_out - x) < 1e-16, arr.ind = TRUE)
  which_y_is_min = which(abs(pmin_out - y) < 1e-16, arr.ind = TRUE)
  out = rep(NA, length(pmin_out))
  out[which_x_is_min] = "x"
  out[which_y_is_min] = "y"
  return(out)
}

##
# Function
##
normal_between_positions.torus = function(positions, dist_between_positions, bounds, where_evaluate) {
  positions0 = reduce_into_canonical_repr(positions, bounds)
  global_diff = diff_of_positions(positions0)
  
  abs_diff = abs(global_diff)
  sign_diff = sign(global_diff)
  border_minus_abs_diff = complementary_abs_diff(abs_diff, bounds)
  
  # Which.min: abs_diff ("x") or border_minus_abs_diff ("y")
  nb_part = nrow(positions0)
  which.min_out = array(NA, dim = c(nb_part, nb_part, ncol(positions0)))
  for(i in 1:nb_part) {
    for(j in 1:nb_part) {
      which_pmin = which.pmin(abs_diff[i,j,], border_minus_abs_diff[i,j,])
      which.min_out[i,j,] = which_pmin
    }
  }
  
  selected = array(NA, dim = c(nb_part, nb_part, ncol(positions0)))
  selected[which.min_out == "x"] = abs_diff[which.min_out == "x"] * sign_diff[which.min_out == "x"]
  selected[which.min_out == "y"] = border_minus_abs_diff[which.min_out == "y"] * -1 * sign_diff[which.min_out == "y"]
  
  n_positions_list = list()
  for(k in 1:nrow(where_evaluate)) {
    selected_k = sweep(selected, 3, as.numeric(where_evaluate[k,]), FUN = "+")
    n_positions = array(NA, dim = c(nb_part, nb_part, ncol(positions0)))
    for(i in 1:nb_part) {
      for(j in 1:nb_part) {
        n_positions[i,j,] = selected_k[i,j,] / dist_between_positions[[k]][i, j]
      }
    }
    n_positions_list[[k]] = n_positions
  }
  
  return(n_positions_list)
}

##
# Example
##
positions = matrix(c(0.9, 0.9,
                     0.1, 0.2,
                     0.0, 0.0,
                     9.1, -9.1),
                   ncol = 2, byrow = T)
bounds = c(1, 1)
sum_elem = 0L
where_evaluate = where_evaluate_func(bounds, sum_elem)
dist_between_positions = dist_between_positions.torus(positions, bounds, where_evaluate)
round(normal_between_positions.torus(positions, dist_between_positions, bounds, where_evaluate)[[1]], 2)
# As shown in the example for the function dist_between_positions.torus,
# a couple of position giving the smallest distance is as follows, for each
# pair:
# For (1)<->(2), (1) is (-0.1,-0.1) and (2) is (0.1,0.2)
# For (1)<->(3), (1) is (-0.1,-0.1) and (3) is (0,0)
# For (1)<->(4), (1) is (-0.1,0.9) and (4) is (0.1,0.9)
# For (2)<->(3), (2) is (0.1,0.2) and (3) is (0,0)
# For (2)<->(4), (2) is (0.1,0.2) and (4) is (0.1,-0.1)
# For (3)<->(4), (3) is (0,0) and (4) is (0.1,-0.1)
# Then the vector of difference from (0,0) is as follows:
# For (1)->(2), position must go from (0,0) to (0.2,0.3)
# For (1)->(3), position must go from (0,0) to (0.1,0.1)
# For (1)->(4), position must go from (0,0) to (0.2,0)
# For (2)->(3), position must go from (0,0) to (-0.1,-0.2)
# For (2)->(4), position must go from (0,0) to (0,-0.3)
# For (3)->(4), position must go from (0,0) to (0.1,-0.1)
# For (2)->(1), position must go from (0,0) to (-0.2,-0.3)
# For (3)->(1), position must go from (0,0) to (-0.1,-0.1)
# For (4)->(1), position must go from (0,0) to (-0.2,0) 
# For (3)->(2), position must go from (0,0) to (0.1,0.2)
# For (4)->(2), position must go from (0,0) to (0,0.3)
# For (4)->(3), position must go from (0,0) to (-0.1,0.1)
# Conclusion: The unit vectors are:
# For (1)->(2), (0.2,0.3)/sqrt(0.2^2+0.3^2) i.e. (0.55, 0.83)
# For (1)->(3), (0.1,0.1)/sqrt(0.1^2+0.1^2) i.e. (0.71, 0.71)
# For (1)->(4), (0.2,0)/sqrt(0.2^2+0^2) i.e. (1, 0)
# For (2)->(3), (-0.1,-0.2)/sqrt(0.1^2+0.2^2) i.e. (-0.45, -0.89)
# For (2)->(4), (0,-0.3)/sqrt(0^2+0.3^2) i.e. (0, -1)
# For (3)->(4), (0.1,-0.1)/sqrt(0.1^2+0.1^2) i.e. (0.71, -0.71)
# For (2)->(1), (-0.2,-0.3)/sqrt(^2+0.3^2) i.e. (-0.55, -0.83)
# For (3)->(1), (-0.1,-0.1)/sqrt(0.1^2+0.1^2) i.e. (-0.71, -0.71)
# For (4)->(1), (-0.2,0)/sqrt(0.2^2+0^2) i.e. (-1, 0)
# For (3)->(2), (0.1,0.2)/sqrt(0.1^2+0.2^2) i.e. (0.45, 0.89)
# For (4)->(2), (0,0.3)/sqrt(0^2+0.3^2) i.e. (0, 1)
# For (4)->(3), (-0.1,0.1)/sqrt(0.1^2+0.1^2) i.e. (-0.71, 0.71)
# As an array:
#   NaN  0.55  0.71     1
# -0.55   NaN -0.45     0        X
# -0.71  0.45   NaN  0.71
#    -1     0 -0.71   NaN
#   
#   NaN  0.83  0.71     0
# -0.83   NaN -0.89    -1        Y
# -0.71  0.89   NaN -0.71
#     0     1  0.71   NaN
rm(positions, bounds, dist_between_positions)

##
# Unit test
##
positions = matrix(c(0.9, 0.9,
                     0.1, 0.2,
                     0.0, 0.0,
                     9.1, -9.1),
                   ncol = 2, byrow = T)
bounds = c(1, 1)
sum_elem = 0L
where_evaluate = where_evaluate_func(bounds, sum_elem)
dist_between_positions = dist_between_positions.torus(positions, bounds, where_evaluate)
checkEqualsNumeric(round(normal_between_positions.torus(positions, dist_between_positions, bounds, where_evaluate)[[1]], 2),
                   array(c(NaN,-0.55,-0.71,-1, # by column (X)
                           0.55, NaN, 0.45, 0,
                           0.71, -0.45, NaN, -0.71,
                           1, 0, 0.71, NaN, NaN,
                           -0.83, -0.71, 0, 0.83, # by column (Y)
                           NaN, 0.89, 1, 0.71,
                           -0.89, NaN, 0.71, 0,
                           -1, -0.71, NaN), dim = c(4,4,2)))
rm(positions, bounds, dist_between_positions)
