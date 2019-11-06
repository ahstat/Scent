

###################################
# Evaluation modulo `bounds` -- 2 #
###################################

f = f_derivdnorm
#f_  function(x) {sign(x) * f(abs(x))}

x = c(0, 0, 0)

f.torus(f, c(0,0,0.1))
f.torus = function(f, x, torus_dim = c(1, 1, 1), sum_elem = 3L) {
  # torus_dim can contain +Inf elements also
  x = matrix(x, ncol = ncol(positions), nrow = 1)
  positions = where_evaluate(torus_dim, sum_elem)
  D = rdist(positions, x)
  fD = -structure(vapply(D, f, numeric(1)), dim=dim(D)) # same as in one_step.R. Could be simplified.
  N = as.matrix(sweep(positions, 2, x) / D) # https://stackoverflow.com/questions/24520720
  out = apply(N * matrix(rep(fD, ncol(N)), ncol = ncol(N)), 2, sum, na.rm = TRUE)
  out = as.numeric(out)
  out[abs(out) < 2e-16] <- 0
  return(out)
}

plot(sapply(seq(from = -10, to = 10, length.out = 1000), function(x) {f.torus(f, c(0,0,x))})[3,])











# torus_dim = options$torus_dim
# 
# 
# rdist(positions)
# 
# 
# normal_between_positions.torus(positions_where_evaluate, d_positions, options$torus_dim)
# 
# 
# DN = dn_positions(positions_where_evaluate, manifold, options = list(torus_dim = c(1,1,1)))
# 
# **..
# 
# 
# sapply(x + df_where_evaluate, f_derivdnorm))
# 
# sum(sapply(0.7 + (-100:100), ))
