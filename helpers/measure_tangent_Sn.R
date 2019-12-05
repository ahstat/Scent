LogS = function(A, B) {
  # Convert a point B of S^{n-1} into a point on the tangent space in A
  # From http://people.csail.mit.edu/jstraub/download/straub2015dptgmm.pdf
  theta = great_circle_distance(A, B)
  if(theta == 0) {
    B_on_tanA = B - A
  } else if(theta == pi) {
    B_on_tanA = NA
    #stop("Points A and B are on the opposite each other")
  } else {
    B_on_tanA = (B - A * cos(theta)) * (theta / sin(theta))
  }
  return(B_on_tanA)
}

LogS_weighted = function(A, B, Df) {
  # LogS is the point B seen on the tangent space in A
  # The distance between A and B_on_tanA on the tangent space is the same with
  # the distance between A and B on the sphere
  # In all cases, it is theta = great_circle_distance(A, B)
  #
  # The weight for movement is proportional to Df(theta)
  # So we get a unit vector by dividing by theta, then multiply with Df(theta), giving:
  # LogS(A, B) * (Df(theta) / theta)
  #
  # We rewrite the function to manage corner case: For antipodal point, we have 0*NA = 0
  theta = great_circle_distance(A, B)
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
    B_weighted_on_tanA = (B - A * cos(theta)) * (Df(theta) / sin(theta))
  }
  return(B_weighted_on_tanA)
}

ExpS = function(A, B_on_tanA) {
  # Convert a point on the tangent space in A to a point B of S^{n-1}
  # From http://people.csail.mit.edu/jstraub/download/straub2015dptgmm.pdf
  norm_B_on_tanA = c(norm_Eucl_vec(B_on_tanA))
  if(norm_B_on_tanA != 0) {
    B = A * cos(norm_B_on_tanA) + (B_on_tanA / norm_B_on_tanA) * sin(norm_B_on_tanA)
  } else {
    B = A
  }
  return(B)
}

# A = c(0, 1, 0)
# B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
# # Weighted contribution of B on A on the tangent plane
# B_weighted_on_tanA = LogS_weighted(A, B, Df)
# # Weighted contribution after going back to the sphere
# B_weighted = ExpS(A, B_weighted_on_tanA)
# 
# great_circle_distance(A, B_weighted)
# 
# theta = great_circle_distance(A, B)
# Df(theta)
