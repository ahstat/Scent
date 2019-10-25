# Recipe for a ddensity:
# * Only need to be defined on nonnegative real values
# * checknormalize function should give 1 if you want the integral after
# making it symmetric to be a density

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
}

if(debug) {
  plot_ddensity()
}
