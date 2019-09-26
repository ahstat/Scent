# Recipe for a ddensity:
# * Only need to be defined on nonnegative real values
# * checknormalize function should give 1 if you want the integral after
# making it symmetric to be a density

##########################################################
# Option 1: Quick approximation of the normal derivative #
##########################################################
f_approxderivdnorm = function(dxy) {
  - pmax(- abs(1 - dxy) + 1, 0) / 2
}
# x = seq(-4,4,length.out = 1000); plot(x, f(x))

###############################
# Option 2: Normal derivative #
###############################
deriv_dnorm = function(x, m = 0) {
  out = - (1 / (2*pi)^(length(m)/2)) * exp(-sum((x-m)^2)/2) * (x - m)
  return(out)
  # # With general Sigma, for reference
  # # Matrix cookbook 8.1.1
  # Sigma = diag(length(m))
  # out = - dmvnorm(x, m, Sigma) * (solve(Sigma) %*% (x - m))
  # return(out)
  
  # In dimension 1, for reference:
  # variance = Sigma[1,1]
  # out = - dnorm(x, m, sqrt(variance)) * (variance^(-1) * (x - m))
}

f_derivdnorm = function(x) {
  if(x >= 0) {
    return(deriv_dnorm(x))
  } else {
    return(0)
  }
}
# x = seq(-4,4,length.out = 1000); plot(x, sapply(x, f_derivdnorm))

##############################################
# Bestiary of the derivatives of the density #
##############################################
plot_ddensity = function() {
  x = seq(-4,4,length.out = 1000)
  plot(x, f_approxderivdnorm(x), type = "l",
       xlab = "x", ylab = "ddensity(x)",
       main = "f the derivative of the density (for x > 0)")
  lines(x, sapply(x, f_derivdnorm), col = "red")
  legend("bottomleft", legend=c("approx dnorm", "dnorm"),
         col=c("black", "red"), lty=c(1, 1), cex=0.8)
  
  Fx = cumsum(sapply(x, function(x){sign(x)*f_approxderivdnorm(abs(x))}))*(x[2] - x[1])
  Fx2 = cumsum(sapply(x, function(x){sign(x)*f_derivdnorm(abs(x))}))*(x[2] - x[1])
  plot(x, Fx, type = "l", 
       xlab = "x", ylab = "density(x)",
       main = "Density obtained after integration of the derivative f")
  lines(x, Fx2, col = "red")
  legend("topleft", legend=c("approx dnorm", "dnorm"),
         col=c("black", "red"), lty=c(1, 1), cex=0.8)
  
  # Checking our ddensities are densities in dimension 1 after integration 
  check_f(f_approxderivdnorm, dim = 1,
          x_such_as_with_f_of_x_is_tiny = 4,
          the_greater_the_more_precise = 1000)
  check_f(f_derivdnorm, dim = 1,
          x_such_as_with_f_of_x_is_tiny = 4,
          the_greater_the_more_precise = 1000)
  return("Done")
}

#################################
# Helper: Normalization machine #
#################################
normalize_approx = function(f, dim,
                            x_such_as_with_f_of_x_is_tiny = 10,
                            the_greater_the_more_precise = 10000) {
  # Compute the constant C to normalize the derivative f such that
  # the integral F(x) = \int_{-\infty}^{x} f(t)dt is such that: 
  # \int_{-\infty}^{+\infty} f(||X||)dX = 1, where X is in R^dim
  # Note: Only valid for the Euclidian distance on R^dim.
  x = seq(from = -x_such_as_with_f_of_x_is_tiny,
          to = x_such_as_with_f_of_x_is_tiny,
          length.out = 2*the_greater_the_more_precise - 1)
  
  # The function f is defined for nonnegative number, but should be symmetric
  # So we extend it:
  f_ext = function(x){sign(x)*f(abs(x))}
  #plot(x, sapply(x, f_ext))
  
  Fx = cumsum(sapply(x, f_ext))*(x[2] - x[1])
  #plot(x, Fx)
  #lines(x, dnorm(x), col = "red") # if f = deriv_dnorm
  
  r = x[which(x >= 0)]
  Fr = Fx[which(x >= 0)]
  # r = seq(from = 0, 
  #         to = x_such_as_with_f_of_x_is_tiny, 
  #         length.out = the_greater_the_more_precise)
  
  left_elem = (2 * pi^(dim/2)) / gamma(dim/2)
  right_elem = sum(r^(dim-1) * Fr * (r[2] - r[1]))
  
  return(left_elem * right_elem)
}

check_f = function(f, dim,
                   x_such_as_with_f_of_x_is_tiny = 10,
                   the_greater_the_more_precise = 10000) {
  C = normalize_approx(f, dim, 
                       x_such_as_with_f_of_x_is_tiny, 
                       the_greater_the_more_precise)
  if(C > 0.95 && C < 1.05) {
    return("OK")
  } else {
    print(paste0("You need to define g: f / C with C = ", C))
    return("not OK")
  }
}
