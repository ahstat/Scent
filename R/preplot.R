##########################################################
# Preplot for spherical (used for plotting and checking) #
##########################################################

##
# Unit vector of B on the Log_A space
##
.deriv_rotated = function(A, B) {
  .Log_weighted_S(A, B, function(x){x}) / .distance_S_great_circle(A, B)
}

##
# Rotation on the great circle from A to B on the sphere, at distance t from A
##
.rotated = function(A, B, t) {
  .Exp_S(A, .deriv_rotated(A, B) * t)
}

##
# Old functions for unit testing only (without Log and Exp)
##
.deriv_rotated_old = function(A, B) {
  # Derivative of the action of B on A on the sphere (see rotated for more info)
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
  if(.distance_S_great_circle(B, B_prim) > pi/2) {
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

.rotated_old = function(A, B, t) {
  # Rotation at constant speed on the great circle from A to B on the sphere
  # A and B are points of R^d belonging to S^{d-1}
  # The function outputs a point of R^d belonging to S^{d-1} which is the
  # rotation from A in the direction B on a distance of t
  # A: Initial point
  # B: Final point after rotation of angle t in the direction A
  B_prim = .deriv_rotated_old(A, B)

  # Rotated point from A to B, given A and the derivative of action of B on A
  rotated_out = cos(t) * A + sin(t) * B_prim # rotated_from_derivative

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
