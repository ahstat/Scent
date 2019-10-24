###############################
# Option 2: Normal derivative #
###############################
deriv_dnorm = function(x, dim = 1) {
  out = - ((1 / (2*pi)^(1/2)) * exp(-sum(x^2)/2) * x) / C_deriv_dnorm(dim)
  return(out)
}

C_deriv_dnorm = function(dim = 1) {
  return((2*pi)^((dim-1)/2))
}

f_derivdnorm_dim = function(dim) {
  return(
    function(x) {
      if(x >= 0) {
        return(deriv_dnorm(x, dim))
      } else {
        return(0)
      }
    }
  )
}

f_derivdnorm = f_derivdnorm_dim(1)
# Note: the normalization constant only slow down the complete behavior
# but does not change the trajectory. So it is not necessary to normalize.
f_derivdnorm1 = f_derivdnorm_dim(1)
f_derivdnorm2 = f_derivdnorm_dim(2)
f_derivdnorm3 = f_derivdnorm_dim(3)
f_derivdnorm4 = f_derivdnorm_dim(4)
f_derivdnorm5 = f_derivdnorm_dim(5)
# check_f(f_derivdnorm1, 1)
# check_f(f_derivdnorm2, 2)
# check_f(f_derivdnorm3, 3)
# check_f(f_derivdnorm4, 4)
# check_f(f_derivdnorm5, 5)

# x = seq(-4,4,length.out = 1000); plot(x, sapply(x, f_derivdnorm))
# deriv_dnorm = function(x, m = 0) {
#   out = - (1 / (2*pi)^(length(m)/2)) * exp(-sum((x-m)^2)/2) * (x - m)
#   return(out)
#   # # With general Sigma, for reference
#   # # Matrix cookbook 8.1.1
#   # Sigma = diag(length(m))
#   # out = - dmvnorm(x, m, Sigma) * (solve(Sigma) %*% (x - m))
#   # return(out)
#   
#   # In dimension 1, for reference:
#   # variance = Sigma[1,1]
#   # out = - dnorm(x, m, sqrt(variance)) * (variance^(-1) * (x - m))
# }