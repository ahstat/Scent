# Measure between two points A, B of H^{d-1}

####################################
# Distance between two points on H #
####################################
.distance_H = function(A, B) {
  inner = .inner_Minkowski(A, B)
  theta = acosh(-inner)
  return(theta)
}

#######
# Log #
#######
.Log_weighted_H = function(A, B, Df) {
  inner = .inner_Minkowski(A, B)
  theta = .distance_H(A, B)
  # B_weighted_on_tanA = (theta / sqrt(inner^2 -1)) * (B + inner * A) * (Df(theta) / theta)
  B_weighted_on_tanA = (Df(theta) / sqrt(inner^2 -1)) * (B + inner * A)
  return(B_weighted_on_tanA)
}

#######
# Exp #
#######
.Exp_H = function(A, B_on_tanA) {
  sq_inner = sqrt(.inner_Minkowski(B_on_tanA, B_on_tanA))
  out = A * cosh(sq_inner) + sinh(sq_inner) * (B_on_tanA / sq_inner)
  # check on hyperbolic space?
  return(out)
}
