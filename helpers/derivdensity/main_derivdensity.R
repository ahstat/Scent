# Recipe for a ddensity:
# * Only need to be defined on nonnegative real values
# * checknormalize function should give 1 if you want the integral after
# making it symmetric to be a density

##############################################
# Bestiary of the derivatives of the density #
##############################################
plot_ddensity = function() {
  x = seq(-4,4,length.out = 1000)
  plot(x, f.approxderivdnorm(x), type = "l",
       xlab = "x", ylab = "ddensity(x)",
       main = "f the derivative of the density (for x > 0)")
  lines(x, sapply(x, f_derivdnorm), col = "red")
  legend("bottomleft", legend=c("approx dnorm", "dnorm"),
         col=c("black", "red"), lty=c(1, 1), cex=0.8)
  
  Fx = cumsum(sapply(x, function(x){sign(x)*f.approxderivdnorm(abs(x))}))*(x[2] - x[1])
  Fx2 = cumsum(sapply(x, function(x){sign(x)*f_derivdnorm(abs(x))}))*(x[2] - x[1])
  plot(x, Fx, type = "l", 
       xlab = "x", ylab = "density(x)",
       main = "Density obtained after integration of the derivative f")
  lines(x, Fx2, col = "red")
  legend("topleft", legend=c("approx dnorm", "dnorm"),
         col=c("black", "red"), lty=c(1, 1), cex=0.8)
  
  # Checking our ddensities are densities in dimension 1 after integration 
  check_f(f.approxderivdnorm, dim = 1,
          x_such_as_with_f_of_x_is_tiny = 4,
          the_greater_the_more_precise = 1000)
  check_f(f_derivdnorm, dim = 1,
          x_such_as_with_f_of_x_is_tiny = 4,
          the_greater_the_more_precise = 1000)
  return("Done")
}

