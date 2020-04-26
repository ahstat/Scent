g_sin = function(angle) {
  -sin(angle)
}
# Note: g_sin is bounded by 1, so the maximum velocity during one unit of time is bounded by 1
# (given all lambda and mu are complex numbers on the unit circle)

g_sin_alpha = function(alpha) {
  function(angle) {
    -sin(alpha * angle)
  }
}

g_ddnorm = function(dim = 1, sigma = 1) {
  # sigma = standard deviation
  coeff = - (2*pi)^(-dim/2) * sigma^(-dim-2)
  return(function(x) {
    coeff * exp(-x^2/(2*sigma^2)) * x
  })
}

g_sinc = function(x) {
  # Derivative of sinc(x) = sin(x)/x
  if(x == 0) {
    return(0)
  } else {
    return((x * cos(x) - sin(x))/x^2)
  }
}
