# Measure between two points A, B of H^{d-1}

########
# Misc #
########
inner_Minkowski = function(A, B) {
  terms = A * B
  positive_terms = 1:(length(terms)-1)
  negative_term = length(terms)
  return(-terms[negative_term] + sum(terms[positive_terms]))
}

normalize_me_hyperbolic = function(A) {
  # Normalize a point of R^d into the element of H^{d-1}
  out = A / sqrt(inner_Minkowski(A, A))
  # can throw an error if A[length(A)] <= 0
  return(out)
}

####################################
# Distance between two points on H #
####################################
distance_H = function(A, B) {
  inner = inner_Minkowski(A, B)
  theta = acosh(-inner)
  return(theta)
}

#######
# Log #
#######
Log_weighted_H = function(A, B, Df) {
  inner = inner_Minkowski(A, B)
  theta = distance_H(A, B)
  # B_weighted_on_tanA = (theta / sqrt(inner^2 -1)) * (B + inner * A) * (Df(theta) / theta)
  B_weighted_on_tanA = (Df(theta) / sqrt(inner^2 -1)) * (B + inner * A)
  return(B_weighted_on_tanA)
}

#######
# Exp #
#######
Exp_H = function(A, B_on_tanA) {
  sq_inner = sqrt(inner_Minkowski(B_on_tanA, B_on_tanA))
  out = A * cosh(sq_inner) + sinh(sq_inner) * (B_on_tanA / sq_inner)
  # check on hyperbolic space?
  return(out)
}
