################################
# Derivative of normal density # (w.r.t. x ; in dimension 1 or >1)
################################

##
# x: position to evaluate
# m: location parameter of the density
# Sigma: matrix of variance-covariance (variance in dimension one)
##

deriv_dnorm = function(x, m, Sigma) {
  # Matrix cookbook 8.1.1
  out = - dmvnorm(x, m, Sigma) * (solve(Sigma) %*% (x - m))
  return(out)
  # In dimension 1, for reference:
  # variance = Sigma[1,1]
  # out = - dnorm(x, m, sqrt(variance)) * (variance^(-1) * (x - m))
}

###################################
# Evaluation modulo `bounds` -- 1 #
###################################
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

where_evaluate = function(x, bounds, sum_elem = 3L) {
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
      xmod[,i] = x[i] + xmod[,i]
    } else {
      xmod[,i] = x[i] + bounds[i] * xmod[,i]
    }
  }
  return(xmod)
}

## Example:
# where_evaluate(c(0,0,0), c(+Inf,2,3), sum_elem = 3L)

###################################
# Evaluation modulo `bounds` -- 2 #
###################################
# The following helper function computes the density:
#    x |--> f(sum{z ; z mod bounds = x})

##
# dcurrent: a density function of the form dcurrent(x, m, Sigma) where:
#    - x: a position to evaluate,
#    - m: location parameter
#    - Sigma: scale parameter
# bounds: bounds length for each dim, as a vector of length `dim`
# sum_elem: number of elements to consider on left and right
##

get_f_out = function(dcurrent, m, Sigma, bounds, sum_elem) {
  f_out = function(x) {
    xmod = where_evaluate(x, bounds, sum_elem)
    out = apply(xmod, 1, function(x) {dcurrent(x, m, Sigma)})
    if(is.null(dim(out)[1])) { # 1D = Evaluation of a density
      return(sum(out))
    } else { # 2D or more = Evaluation of the derivative
      return(apply(out, 1, sum))
    }
  }
  return(f_out)
}

## Examples
# get_f_out(dmvnorm, c(0,0,0), diag(3), c(1,2,3), 3L)(c(0, 0, 0))
# get_f_out(dmvnorm, c(0), diag(1), c(2), 3L)(c(0))
# 
# get_f_out(deriv_dnorm, c(0,0,0), diag(3), c(1,2,3), 3L)(c(0, 0, 0))
# get_f_out(deriv_dnorm, c(0,0,0), diag(3), c(+Inf, +Inf, +Inf), 3L)(c(0, 0, 0))
# get_f_out(deriv_dnorm, c(0,0,0), diag(3), c(+Inf, +Inf, +Inf), 3L)(c(1, 0, 0))
# 
# get_f_out(deriv_dnorm, c(0), diag(1), c(+Inf), 3L)(0)
# get_f_out(deriv_dnorm, c(0), diag(1), c(+Inf), 3L)(1)

##################
# Normal density #
##################
f = function(m = rep(0, 3), sigma = diag(3), bounds = c(+Inf, +Inf, +Inf), sum_elem = 10L) {
  return(get_f_out(dmvnorm, m, sigma, bounds, sum_elem))
}

####################################
# Derivative of the Normal density #
####################################
Df = function(m = rep(0, 3), sigma = diag(3), bounds = c(+Inf, +Inf, +Inf), sum_elem = 10L) {
  return(get_f_out(deriv_dnorm, m, sigma, bounds, sum_elem))
}
