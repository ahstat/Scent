## Normal density
f = function(m = rep(0, 2), sigma = diag(2), bounds = c(-Inf, +Inf), sum_elem = 10L) {
  dim = length(m)
  if(dim == 1L) {
    dcurrent = dnorm
  } else {
    dcurrent = dmvnorm
  }
  return(get_f_out(dcurrent, m, sigma, bounds, sum_elem))
}

## Derivative of the Normal density
Df = function(m = rep(0, 2), sigma = diag(2), bounds = c(+Inf, +Inf), sum_elem = 10L) {
  return(get_f_out(deriv_dnorm, m, sigma, bounds, sum_elem))
}

## Helper 3: Computing the density in x as the sum in all bounded directions
get_f_out = function(dcurrent, m, sigma, bounds, sum_elem) {
  f_out = function(x) {
    if(length(x) != length(m)) {
      stop("x and m must have same dimensions")
    }
    xmod = where_evaluate(x, bounds, sum_elem)
    out = apply(xmod, 1, function(x) {dcurrent(x, m, sigma)})
    if(is.null(dim(out)[1])) { # 1D
      return(sum(out))
    } else { # 2D or more
      return(apply(out, 1, sum))
    }
  }
  return(f_out)
}

## Helper 4: Define where are points x to evaluate in order to get the
# global density, in case of bounded directions
where_evaluate = function(x, bounds, sum_elem = 3L) {
  my_seq = seq(from = -sum_elem, to = sum_elem, by = 1L)
  dim = length(bounds)
  # Source: helpers/input_array.R for triangle-pursuit code
  list_to_expand = list()
  for(i in 1:dim) {
    if(is.infinite(bounds[i])) {
      list_to_expand[[i]] = 0
    } else {
      list_to_expand[[i]] = my_seq
    }
  }
  xmod = expand.grid(list_to_expand)
  for(i in 1:dim) {
    if(is.infinite(bounds[i])) {
      xmod[,i] = x[i] + xmod[,i]
    } else {
      xmod[,i] = x[i] + bounds[i] * xmod[,i]
    }
  }
  return(xmod)
}
