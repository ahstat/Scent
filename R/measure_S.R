# Measure between two points A, B of S^{d-1}

####################################
# Distance between two points on S #
####################################
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

#######
# Log #
#######
.Log_weighted_S = function(A, B, Df) {
  # Convert a point B of S^{d-1} into a point on the tangent space in A
  # From http://people.csail.mit.edu/jstraub/download/straub2015dptgmm.pdf
  # Then, multiply by Df(theta) / theta to weight it.
  # For removing weights, take Df: theta |--> theta

  # LogS is the point B seen on the tangent space in A (with A the origin)
  # The distance between A and B_on_tanA on the tangent space is the same with
  # the distance between A and B on the sphere
  # In all cases, it is theta = .distance_S_great_circle(A, B)
  #
  # The weight for movement is proportional to Df(theta)
  # So we get a unit vector by dividing by theta, then multiply with Df(theta), giving:
  # Log_weighted_S(A, B, Df) = Log_S(A, B) * (Df(theta) / theta)
  #
  # We rewrite the function to manage corner case: For antipodal point, we have 0*NA = 0
  theta = .distance_S_great_circle(A, B)
  if(theta == 0) {
    B_weighted_on_tanA = rep(0, length(B)) # which is Df(theta) * 0
  } else if(theta == pi) {
    if(abs(Df(theta)) > 1e-10) {
      print(theta)
      print(Df(theta))
      stop("Points A and B are on the opposite each other but action Df is not 0")
    } else {
      B_weighted_on_tanA = rep(0, length(B)) # which is Df(theta) * anything
    }
  } else {
    # B_weighted_on_tanA = (B - A * cos(theta)) * (theta / sin(theta)) * (Df(theta) / theta)
    B_weighted_on_tanA = (B - A * cos(theta)) * (Df(theta) / sin(theta))
  }
  return(B_weighted_on_tanA)
}

#######
# Exp #
#######
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
