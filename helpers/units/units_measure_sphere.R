A = c(0, 1, 0)
B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
# Weighted contribution of B on A on the tangent plane
B_weighted_on_tanA = Log_weighted_S(A, B, Df)
# Weighted contribution after going back to the sphere
B_weighted = Exp_S(A, B_weighted_on_tanA)

distance_S_great_circle(A, B_weighted)

theta = distance_S_great_circle(A, B)
Df(theta)


# For plot, should be archive...

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
  if(distance_S_great_circle(B, B_prim) > pi/2) {
    stop("The first solution is not OK")
    # Take the other solution if B_prim is distant from B
    # B_prim = -B_prim will work in this case
  }
  return(B_prim)
  # Explanation (read explanation of `rotated` function first):
  # Here we know the rotation of distance t is cos(t) * A + sin(t) * B_prim
  # When t --> 0 we go rotated(A, B, 0)=A
  # So the derivative is the limit in 0 of (rotated(A, B, t) - A) / t which is B_prim
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
  # A and B are points of R^d belonging to S^{d-1}
  # The function outputs a point of R^d belonging to S^{d-1} which is the
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
