###############################################################
# Sample uniform distribution on the surface of a unit sphere #
###############################################################
sample_surface_sphere = function(n_elem, dim_S = 2, seed = 1234) {
  # Pick n_elem points on S^{n-1} (20180501)
  # http://mathworld.wolfram.com/HyperspherePointPicking.html
  set.seed(seed)
  n = dim_S + 1
  M = matrix(rnorm(n * n_elem), ncol = n)
  sample = M / sqrt(apply(M^2, 1, sum))
  return(sample)
}
