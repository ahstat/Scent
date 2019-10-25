##########################################################
# Option 1: Quick approximation of the normal derivative #
##########################################################
f_approxderivdnorm = function(x) {
  - pmax(- abs(1 - x) + 1, 0) / 2
}
# x = seq(-4,4,length.out = 1000); 
# plot(x, sapply(x, f_approxderivdnorm), type = "l")