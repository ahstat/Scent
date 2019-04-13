## Mix function: takes 1 density, means, sigmas
mix_func = function(f, m = 0, sigma = 1, bounds, sum_elem) {
  function(x) {sapply(x, function(x){mean(f(m, sigma, bounds, sum_elem)(x))})}
}

## Pushing one step
push = function(m, type, alpha, Df, sigma, bounds, sum_elem) {
  Dg = mix_func(Df, m, sigma, bounds, sum_elem)
  m = m + type * alpha * Dg(m)
  return(m)
}

## Plotting
plotting = function(x, m, type, f, Df, sigma, bounds, sum_elem, with_deriv = FALSE, ...) {
  g = mix_func(f, m, sigma, bounds, sum_elem)
  Dg = mix_func(Df, m, sigma, bounds, sum_elem)
  
  if(is.null(x)) {
    x = seq(from = -3, to = 3, length.out = 1000)
  }
  
  if(with_deriv) {
    ylim = range(c(g(x), Dg(x)))
    plot(x, g(x), type = "l", ylim = ylim, ...)
    lines(x, Dg(x), col = "red")
    points(m, g(m), type = "p", col = "black")
    points(m, Dg(m), type = "p", col = "red")
  } else {
    plot(x, g(x), type = "l", ...)
    points(m, g(m), type = "p", col = ifelse(type == 1, "blue", "red"))
    points(m - 2*pi, g(m), type = "p", col = "black")
    points(m + 2*pi, g(m), type = "p", col = "black")
  }
}