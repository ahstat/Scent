# Scent

# https://rdrr.io/cran/NPflow/man/mvnpdfC.html
# Higher density derivative for normal distribution in any dimension?
# In dim 1: exp(-1/2)*sqrt(1/(2*pi)) = 1/sqrt(2*pi*exp(1))
# In dim n: 
# https://github.com/mfasiolo/mvnfast
# https://www.gnu.org/software/gsl/manual/html_node/The-Multivariate-Gaussian-Distribution.html

1/(2*pi*e)^(N/2)

N/(2*pi*e)^N


N = 1:10
plot(N, N/(2*pi*exp(1))^N)
plot(N, 1/(N/(2*pi*exp(1))^N))


# retrieve equ diff



###############################################################################

# Set mixture locally, neglect if > threshold.

# Using acceleration instead of speed (but changing the concept)