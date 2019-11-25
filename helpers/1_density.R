################################
# Derivative of normal density # (w.r.t. x ; in dimension 1 or >1)
################################

# Used for moving along the gradient

##
# x: position to evaluate
# m: location parameter of the density
##

deriv_dnorm = function(x, m) {
  out = - (1 / (2*pi)^(length(m)/2)) * exp(-sum((x-m)^2)/2) * (x - m)
  return(out)
}

###########################
# Value of normal density # (w.r.t. x ; in dimension 1 or >1)
###########################

# Used for visualization only

##
# x: position to evaluate
# m: location parameter of the density
##

value_dnorm = function(x, m) {
  Sigma = diag(length(m))
  return(dmvnorm(x, m, Sigma))
}

##################################
# Pre evaluation modulo `bounds` #
##################################

# The following helper function defines the set {z ; z mod bounds = 0}

# In context:
# Let f be the normal density in dimension one.
# If we specify the bound to 2 (corresponding to [0,2] for example), 
# since we get a torus,
# evaluation of the density in x=0 corresponds to the sum:
#    f(0) + f(2) + f(-2) + f(-4) + f(4) + ... = sum[f(z) ; z mod 2 = 0]

##
# x: position for which the set is created, living in dimension `dim`
# bounds: bounds length for each dim, as a vector of length `dim`
# sum_elem: number of elements to consider on left and right
##

## Example:
# pre_where_evaluate_func(c(+Inf,2,3), sum_elem = 3L)

pre_where_evaluate_func = function(bounds, sum_elem = 3L) {
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
  
  # Arrange such that xmod[[1]] is the no-move, and by distance
  xmod$dist = apply(xmod, 1, norm, type = "2")
  xmod = xmod %>% arrange(dist) %>% select(-dist)
  
  return(xmod)
}

###################################
# Evaluation modulo `bounds` -- 1 #
###################################
# The following helper function defines the set {z ; z mod bounds = x}

##
# x: position for which the set is created, living in dimension `dim`
# pre_where_evaluate: translated elements to evaluate given by pre_where_evaluate_func
##

## Example:
# pre_where_evaluate = pre_where_evaluate_func(c(+Inf,2,3), sum_elem = 3L)
# where_evaluate(c(0,0,0), pre_where_evaluate)

where_evaluate = function(x, pre_where_evaluate) {
  sweep(pre_where_evaluate, 2, x, "+")
}

###################################
# Evaluation modulo `bounds` -- 2 #
###################################
# The following helper function computes the density:
#    x |--> f(sum{z ; z mod bounds = x})

##
# dcurrent: a density function of the form dcurrent(x, m, Sigma) where:
#    - x: a position to evaluate,
#    - m: location parameter
# pre_where_evaluate: translated elements to evaluate given by pre_where_evaluate_func
##

get_f_out = function(dcurrent, m, pre_where_evaluate) {
  f_out = function(x) {
    xmod = where_evaluate(x, pre_where_evaluate)
    out = apply(xmod, 1, function(x) {dcurrent(x, m)})
    if(is.null(dim(out)[1])) { # 1D = Evaluation of a density
     return(sum(out))
    } else { # 2D or more = Evaluation of the derivative
      return(apply(out, 1, sum))
    }
  }
  return(f_out)
}

## Examples
get_f_out(dmvnorm, c(0,0,0), pre_where_evaluate_func(c(1,2,3), 3L) )(c(0, 0, 0))
get_f_out(dmvnorm, c(0), pre_where_evaluate_func(c(2), 3L))(c(0))

get_f_out(deriv_dnorm, c(0,0,0), pre_where_evaluate_func(c(1,2,3), 3L))(c(0, 0, 0))
get_f_out(deriv_dnorm, c(0,0,0), pre_where_evaluate_func(c(+Inf, +Inf, +Inf), 3L))(c(0, 0, 0))
get_f_out(deriv_dnorm, c(0,0,0), pre_where_evaluate_func(c(+Inf, +Inf, +Inf), 3L))(c(1, 0, 0))

get_f_out(deriv_dnorm, c(0), pre_where_evaluate_func(c(+Inf), 3L))(0)
get_f_out(deriv_dnorm, c(0), pre_where_evaluate_func(c(+Inf), 3L))(1)

##################
# Normal density #
##################
f = function(m = rep(0, 3), pre_where_evaluate = pre_where_evaluate_func(c(+Inf, +Inf, +Inf), 10L)) {
  return(get_f_out(value_dnorm, m, pre_where_evaluate))
}

####################################
# Derivative of the Normal density #
####################################
Df = function(m = rep(0, 3), pre_where_evaluate = pre_where_evaluate_func(c(+Inf, +Inf, +Inf), 10L)) {
  return(get_f_out(deriv_dnorm, m, pre_where_evaluate))
}
