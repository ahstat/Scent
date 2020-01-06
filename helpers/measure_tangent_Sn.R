# Measure between two points A, B of S^{n-1}

############
# Get norm #
############
norm_Eucl_vec = function(A) {
  # Return ||A|| of a point of R^n
  # https://stackoverflow.com/questions/10933945/
  sqrt(crossprod(A))
}

normalize_me = function(A) {
  # Normalize a point of R^n into the element of S^{n-1}
  A / c(norm_Eucl_vec(A))
}

inner_Minkowski = function(A, B) {
  terms = A * B
  positive_terms = 1:(length(terms)-1)
  negative_term = length(terms)
  return(-terms[negative_term] + sum(terms[positive_terms]))
}

normalize_me_Hyperbolic = function(A) {
  # Normalize a point of R^n into the element of H^{n-1}
  out = A / sqrt(inner_Minkowski(A, A))
  # can throw an error if A[length(A)] <= 0
  return(out)
}



#############################################
# Distance between two points of the sphere #
#############################################
great_circle_distance = function(A, B) {
  # Points are elements of R^n belonging to S^{n-1}
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
################################################################################
# Derivative of the action of B on A on the sphere (see rotated for more info) #
################################################################################
deriv_rotated = function(A, B) {
  Lambda = c(crossprod(A, B))
  if(Lambda == 1) {
    return(NA)
    #stop("Points A and B are identical")
  }
  if(Lambda == -1) {
    return(NA)
    #stop("Points A and B are on the opposite each other")
  }
  B_prim = (B - Lambda * A) / sqrt(1 - Lambda^2)
  if(great_circle_distance(B, B_prim) > pi/2) {
    stop("The first solution is not OK")
    # Take the other solution if B_prim is distant from B
    # B_prim = -B_prim will work in this case
  }
  return(B_prim)
  # Explanation (read explanation of `rotated` function first):
  # Here we know the rotation of distance t is cos(t) * A + sin(t) * B_prim
  # When t --> 0 we go rotated(A, B, 0)=A
  # So the derivative is the limit in 0 of (rotated(A,B,t)-A)/t which is B_prim
}

#############################################################################
# Rotated point from A to B, given A and the derivative of action of B on A #
#############################################################################
rotated_from_derivative = function(A, B_prim, t) {
  return(cos(t) * A + sin(t) * B_prim)
  # Explanation (read explanation of `rotated` function)
}

############################################################################
# Rotation at constant speed on the great circle from A to B on the sphere #
############################################################################
rotated = function(A, B, t) {
  # A and B are points of R^n belonging to S^{n-1}
  # The function outputs a point of R^n belonging to S^{n-1} which is the
  # rotation from A in the direction B on a distance of t
  # A: Initial point
  # B: Final point after t = pi/2
  # So we should have:
  # * rotated(A, B, 0) gives A
  # * rotated(A, B, pi/2) gives B_prim (such that <A|B_prim>=0 and A,B,B_prim on the same circle)
  # * rotated(A, B, pi) gives -A
  # * rotated(A, B, 3*pi/2) gives -B_prim
  B_prim = deriv_rotated(A, B)
  rotated_out = rotated_from_derivative(A, B_prim, t)
  return(rotated_out)
  # Explanation:
  # We suppose A and B are not a pole each other
  # Step 1: Find B' on the great circle A <--> B such that <A|B'>=0 [there are 2 B' like this]
  # Find B' of the form: B' = cos(t0) A + sin(t0) B for a certain t0
  # 0 = <A|B'> = <A|cos(t0) A + sin(t0) B> = cos(t0) + sin(t0) <A|B>
  # - cos(t0)/sin(t0) = <A|B>
  # tan(t0) = -1/<A|B>
  # t_1 = atan(-1/<A|B>) + pi or t_2 = atan(-1/<A|B>)
  # t_1 = -atan(1/<A|B>) + pi or t_2 = -atan(1/<A|B>)
  # t_1 = -pi/2 + atan(<A|B>) or t_2 = pi/2 + atan(<A|B>)
  # So B_prim_1 = cos(t_1) * A + sin(t_1) * B and B_prim_2 = -B_prim_1
  #    B_prim_1 = (<A|B> * A - B) / sqrt(1 + <A|B>^2)
  # And to normalize it is: sqrt((1-<A|B>^2)/(1+<A|B>^2))
  # So B_prim_1 = (<A|B> * A - B) / sqrt(1 - <A|B>^2) and B_prim_2 = -B_prim_1
  #c(crossprod(A, B_prim_1)) # ok == 0
  #c(crossprod(B_prim_1, B_prim_1)) # ok == 1
}

my_matrix = sample_surface_sphere(n_elem = 2, dim_S = 2, seed = 1234)
A = my_matrix[1,]
B = my_matrix[2,]
theta = seq(from = 0, to = 2*pi, length.out = 100)
line_from_A_to_A_by_B = t(sapply(theta, function(t) {rotated(A, B, t)}))
if(!all(round(apply(line_from_A_to_A_by_B, 1, norm_Eucl_vec), 10) == 1)) {
  stop("The output of rotated() is not a point of the unit sphere")
}
if(!all(round(rotated(A, B, 0) - A, 10) == 0)) {
  stop("rotated with t=0 does not give the original point")
}
if(!all(round(rotated(A, B, pi) - (-A), 10) == 0)) {
  stop("rotated with t=pi does not give the opposite point")
}
rm(my_matrix, A, B, theta, line_from_A_to_A_by_B)


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
  # LogS is the point B seen on the tangent space in A (with A the origin)
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

LogS_weighted_Euclidian = function(A, B, Df) {
  theta = c(norm_Eucl_vec(B - A))
  B_weighted_on_tanA = Df(theta) * (B - A) / theta
  return(B_weighted_on_tanA)
}

LogS_weighted_Hyperbolic = function(A, B, Df) {
  inner = inner_Minkowski(A, B)
  theta = acosh(-inner)
  #B_weighted_on_tanA = (theta / sqrt(inner^2 -1)) * (B + inner * A) * (Df(theta) / theta)
  B_weighted_on_tanA = (Df(theta) / sqrt(inner^2 -1)) * (B + inner * A)
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

ExpS_Euclidian = function(A, B_on_tanA) {
  return(A + B_on_tanA)
}

ExpS_Hyperbolic = function(A, B_on_tanA) {
  sq_inner = sqrt(inner_Minkowski(B_on_tanA, B_on_tanA))
  out = A * cosh(sq_inner) + sinh(sq_inner) * (B_on_tanA / sq_inner)
  # check on hyperbolic space?
  return(out)
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
