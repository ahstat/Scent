# On Euclidian
.norm_Eucl_vec = function(A) {
  # Return ||A|| of a point of R^d
  # https://stackoverflow.com/questions/10933945/
  sqrt(crossprod(A))
}

# On Sphere
.normalize_me_on_S = function(A) {
  # Normalize a point of R^d into the element of S^{d-1}
  A / c(.norm_Eucl_vec(A))
}

# On Hyperbolic
.inner_Minkowski = function(A, B) {
  terms = A * B
  positive_terms = 1:(length(terms)-1)
  negative_term = length(terms)
  return(-terms[negative_term] + sum(terms[positive_terms]))
}

.normalize_me_hyperbolic = function(A) {
  # Normalize a point of R^d into the element of H^{d-1}
  out = A / sqrt(.inner_Minkowski(A, A))
  # can throw an error if A[length(A)] <= 0
  return(out)
}
