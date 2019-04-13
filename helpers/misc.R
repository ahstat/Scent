##################
# Misc densities #
##################
## Derivative of the normal density w.r.t. x in dimension 1 or >1
deriv_dnorm = function(x, m, sigma) {
  # dim = length(m)
  # if(dim == 1L) {
  #   # In this case: sigma is sd the standard deviation
  #   dcurrent = dnorm
  #   out = - dcurrent(x, m, sigma) * (sigma^(-2) * (x - m))
  #   return(out)
  # } else if(dim > 1L) {
    # In this case: sigma is matrix of variance-covariance
    dcurrent = dmvnorm
    # Matrix cookbook 8.1.1
    out = - dcurrent(x, m, sigma) * (solve(sigma) %*% (x - m))
    return(out)
  # } else {
  #   stop("m must be a vector of length >= 1")
  # }
}
