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
