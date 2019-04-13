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



## Garbage
#where_evaluate(c(0,0,0), c(1,NA,2), sum_elem = 1)


# Df(0,1,NA)(0)
# Df()(c(1,0))
# x=1;m=2;sigma=3
# Df_norm(1,2,3)

# f(m = rep(0, 3), sigma = diag(3), bounds = c(NA,2,NA), sum_elem = 5L)(c(0,0,0))
# f(m = 0, sigma = 1, bounds = NA, sum_elem = 5L)(0)

# ## Derivative of the Normal density
# Df = function(m = rep(0, 2), sigma = diag(2), bounds = c(NA, NA), sum_elem = 10L) {
#   dim = length(m)
#   if(dim == 1L) {
#     dcurrent = Df_norm
#   } else {
#     print("aaa")
#     dcurrent = Df_norm # TODO # dmvnorm
#   }
#   return(get_f_out(dcurrent, m, sigma, bounds, sum_elem))
# }

# x = seq(from = -pi, pi, length.out = 1000)
# y = sapply(x, function(x){Df(m = 0, sigma = 1, bounds = 2*pi, sum_elem = 10L)(x)})
# plot(x, y)

# dcurrent = Df_norm
# x = c(0,0)
# m = c(0,0)
# sigma = diag(2)
# dcurrent(x, m, sigma)
# 
# Df(m = c(0,0), sigma = diag(2), bounds = c(NA,NA), sum_elem = 3L)(c(0,0))

# ## Wrapped Normal density
# f_wnorm = function(dim = 1L, sum_elem = 10L) {
#   f = function(m = 0, sigma = 1, bounds = NA) {
#     function(x) {
#       if(is.na(NA)) {
#         y = x
#       } else {
#         y = x + bounds * seq(from = -sum_elem, to = sum_elem, by = 1)
#       }
#       f_temp = f_norm(dim)(m, sigma)
#       out = sapply(y, function(x) {f_temp(x)})
#       sum(out)
#     }
#   }
# }
# 
# f_wrapped()
# 
# bounds = 2*pi
# 
# 



# f_norm = function() {
#   f = function(m = 0, sigma = 1) {
#     function(x){dnorm(x, m, sigma)}
#   }
#   return(f)
# }


# f_normw = function(length_circle = 2*pi) {
#   f = function(m = 0, sigma = 1) {
#     function(x) {
#       y = x + length_circle * seq(from = -10L, to = 10L, by = 1)
#       f_temp = f_norm()(m, sigma)
#       out = sapply(y, function(x) {f_temp(x)})
#       sum(out)
#     }
#   }
#   return(f)
# }



# Df_normw = function(length_circle = 2*pi) {
#   Df = function(m = 0, sigma = 1) {
#     function(x) {
#       y = x + length_circle * seq(from = -10L, to = 10L, by = 1)
#       Df_temp = Df_norm()(m, sigma)
#       out = sapply(y, function(x) {Df_temp(x)})
#       sum(out)
#     }
#   }
#   return(Df)
# }

# length_circle = 2*pi
# x = seq(from = -length_circle/2, length_circle/2, length.out = 1000)
# plot(x, sapply(x, function(x){Df_normw(length_circle)()(x)}), type = "l")
# plot(x, sapply(x, function(x){f_normw(length_circle)()(x)}), type = "l")

## Other choices are possible
# Df = function(m = 0, sigma = 1) {
#   function(x) {
#     1/(1e-10 + x^2)
#   }
# }