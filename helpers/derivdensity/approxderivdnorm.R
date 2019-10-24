##########################################################
# Option 1: Quick approximation of the normal derivative #
##########################################################
f.approxderivdnorm = function(dxy) {
  - pmax(- abs(1 - dxy) + 1, 0) / 2
}
# x = seq(-4,4,length.out = 1000); plot(x, f(x))