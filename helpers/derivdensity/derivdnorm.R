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
f_derivdnorm1 = f_derivdnorm_dim(1)
f_derivdnorm2 = f_derivdnorm_dim(2)
f_derivdnorm3 = f_derivdnorm_dim(3)
f_derivdnorm4 = f_derivdnorm_dim(4)
f_derivdnorm5 = f_derivdnorm_dim(5)

# x = seq(-4,4,length.out = 1000);
# plot(x, sapply(x, f_derivdnorm), type = "l")
