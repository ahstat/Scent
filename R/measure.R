Log_weighted_M = function(manifold = "S") {
  if(manifold == "E") {
    return(.Log_weighted_E)
  } else if(manifold == "S") {
    return(.Log_weighted_S)
  } else if(manifold == "H") {
    return(.Log_weighted_H)
  }
}

Exp_M = function(manifold = "S") {
  if(manifold == "E") {
    return(.Exp_E)
  } else if(manifold == "S") {
    return(.Exp_S)
  } else if(manifold == "H") {
    return(.Exp_H)
  }
}

dist_M = function(A, B, manifold = "S") {
  if(manifold == "E") {
    return(.distance_E(A, B))
  } else if(manifold == "S") {
    return(.distance_S_great_circle(A, B))
  } else if(manifold == "H") {
    return(.distance_H(A, B))
  }
}

#############
# Euclidian #
#############

##
# Distance between two points
##
.distance_E = function(A, B) {
  .norm_Eucl_vec(B - A)
}

##
# Log
##
.Log_weighted_E = function(A, B, g) {
  theta = .distance_E(A, B)
  if(theta == 0) {
    if(g(0) == 0) {
      B_weighted_on_tanA = rep(0, length(B)) # which is g(theta) * 0
    } else {
      stop("g(0) must be 0")
    }
  } else {
    # B_weighted_on_tanA = (B - A) * (g(theta) / theta)
    B_weighted_on_tanA = g(theta) * (B - A) / theta
  }
  return(B_weighted_on_tanA)
}

##
# Exp
##
.Exp_E = function(A, B_on_tanA) {
  return(A + B_on_tanA)
}

#############
# Spherical #
#############

# Measure between two points A, B of S^{d-1}

##
# Distance between two points on S
##
.distance_S_great_circle = function(A, B) {
  # Points are elements of R^d belonging to S^{d-1}
  # https://en.wikipedia.org/wiki/N-vector
  Lambda = c(crossprod(A, B))
  if(Lambda > 1) {
    # case of numerical problem, when Lambda = 1.00001
    return(0)
  } else if(Lambda < -1) {
    # case of numerical problem, when Lambda = -1.00001
    return(pi)
  }
  acos(Lambda)
}

##
# Log
##
.Log_weighted_S = function(A, B, g) {
  # Convert a point B of S^{d-1} into a point on the tangent space in A
  # From http://people.csail.mit.edu/jstraub/download/straub2015dptgmm.pdf
  # Then, multiply by g(theta) / theta to weight it.
  # For removing weights, take g: theta |--> theta

  # LogS is the point B seen on the tangent space in A (with A the origin)
  # The distance between A and B_on_tanA on the tangent space is the same with
  # the distance between A and B on the sphere
  # In all cases, it is theta = .distance_S_great_circle(A, B)
  #
  # The weight for movement is proportional to g(theta)
  # So we get a unit vector by dividing by theta, then multiply with g(theta), giving:
  # Log_weighted_S(A, B, g) = Log_S(A, B) * (g(theta) / theta)
  #
  # We rewrite the function to manage corner case: For antipodal point, we have 0*NA = 0
  #
  # Note: you need to check yourelf that A, B are in S!
  theta = .distance_S_great_circle(A, B)
  if(theta == 0) {
    if(g(0) == 0) {
      B_weighted_on_tanA = rep(0, length(B)) # which is g(theta) * 0
    } else {
      stop("g(0) must be 0")
    }
  } else if(theta == pi) {
    if(abs(g(theta)) > 1e-10) {
      print(theta)
      print(g(theta))
      stop("Points A and B are on the opposite each other but action g is not 0")
    } else {
      B_weighted_on_tanA = rep(0, length(B)) # which is g(theta) * anything
    }
  } else {
    # B_weighted_on_tanA = (B - A * cos(theta)) * (theta / sin(theta)) * (g(theta) / theta)
    B_weighted_on_tanA = (B - A * cos(theta)) * (g(theta) / sin(theta))
  }
  return(B_weighted_on_tanA)
}

##
# Exp
##
.Exp_S = function(A, B_on_tanA) {
  # Convert a point on the tangent space in A to a point B of S^{d-1}
  # From http://people.csail.mit.edu/jstraub/download/straub2015dptgmm.pdf
  norm_B_on_tanA = c(.norm_Eucl_vec(B_on_tanA))
  if(norm_B_on_tanA != 0) {
    B = A * cos(norm_B_on_tanA) + (B_on_tanA / norm_B_on_tanA) * sin(norm_B_on_tanA)
  } else {
    B = A
  }
  return(B)
}

##############
# Hyperbolic #
##############

# Measure between two points A, B of H^{d-1}

##
# Distance between two points on H
##
.distance_H = function(A, B) {
  stop("Unit tests not done.")
  inner = .inner_Minkowski(A, B)
  theta = acosh(-inner)
  return(theta)
}

##
# Log
##
.Log_weighted_H = function(A, B, g) {
  stop("Unit tests not done.")
  inner = .inner_Minkowski(A, B)
  theta = .distance_H(A, B)
  # B_weighted_on_tanA = (theta / sqrt(inner^2 -1)) * (B + inner * A) * (g(theta) / theta)
  B_weighted_on_tanA = (g(theta) / sqrt(inner^2 -1)) * (B + inner * A)
  return(B_weighted_on_tanA)
}

##
# Exp
##
.Exp_H = function(A, B_on_tanA) {
  stop("Unit tests not done.")
  sq_inner = sqrt(.inner_Minkowski(B_on_tanA, B_on_tanA))
  out = A * cosh(sq_inner) + sinh(sq_inner) * (B_on_tanA / sq_inner)
  # check on hyperbolic space?
  return(out)
}

########
# Misc #
########

##
# On Euclidian
##
.norm_Eucl_vec = function(A) {
  # Return ||A|| of a point of R^d
  # https://stackoverflow.com/questions/10933945/
  c(sqrt(crossprod(A)))
}

##
# On Sphere
##
.normalize_me_on_S = function(A) {
  # Normalize a point of R^d into the element of S^{d-1}
  if(all(A == 0)) {
    stop("A is 0, cannot project it on the sphere")
  }
  A / .norm_Eucl_vec(A)
}

##
# On Hyperbolic
##
.inner_Minkowski = function(A, B) {
  stop("Unit tests not done.")
  terms = A * B
  positive_terms = 1:(length(terms)-1)
  negative_term = length(terms)
  return(-terms[negative_term] + sum(terms[positive_terms]))
}

.normalize_me_hyperbolic = function(A) {
  stop("Unit tests not done.")
  # Normalize a point of R^d into the element of H^{d-1}
  if(A[length(A)] < 0) {
    # cannot normalize if A[length(A)] <= 0
    A = -A
  }
  out = A / sqrt(.inner_Minkowski(A, A))
  return(out)
}
