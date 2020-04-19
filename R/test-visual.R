# All functions defined in this file are only used in this file and in
# the test-test-visual.R unit tests.
# Only visual_test_X() functions have been exported.

##################################################################
# Testplot for spherical (used for plotting for visual checking) #
##################################################################

##
# Test of .rotated and .deriv_rotated
##
visual_test_1 = function() {
  # Helper for this visual test
  test_segment_on_sphere = function(seed = 1, t_max = "segment") {
    my_matrix = sample_on_S(n_elem = 2, dim_S = 2, seed = seed)

    A = my_matrix[1,]
    B = my_matrix[2,]

    if(t_max == "line") {
      t_max = 2*pi
    } else if(t_max == "segment") {
      t_max = .distance_S_great_circle(A, B)
    } else if(t_max == "semisegment") {
      t_max = .distance_S_great_circle(A, B) / 2
    }

    theta = seq(from = 0, to = t_max, length.out = 100)
    line_from_A_to_B = t(sapply(theta, function(t) {.rotated(A, B, t)}))

    .plot_sphere()
    .plot_path_on_sphere(line_from_A_to_B, col = "black")
    .plot_point_on_sphere(A, "red")
    .plot_point_on_sphere(B, "blue")
  }

  print("Segment from red to blue")
  test_segment_on_sphere(1)
  print("Line passing through (red, blue)")
  test_segment_on_sphere(2, "line")
  print("Segment from red to blue stopping at the middle")
  test_segment_on_sphere(4, "semisegment")
}

visual_test_2 = function() {
  # Helper for this visual test
  test_tangent_on_sphere = function(seed = 1, t_max = "segment") {
    if(seed == 1) {
      A = c(0, 1, 0)
      B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
    } else {
      my_matrix = sample_on_S(n_elem = 2, dim_S = 2, seed = seed)
      A = my_matrix[1,]
      B = my_matrix[2,]
    }

    if(t_max == "line") {
      t_max = 2*pi
    } else if(t_max == "segment") {
      t_max = .distance_S_great_circle(A, B)
    } else if(t_max == "semisegment") {
      t_max = .distance_S_great_circle(A, B) / 2
    }

    theta = seq(from = 0, to = t_max, length.out = 100)
    line_from_A_to_B = t(sapply(theta, function(t) {.rotated(A, B, t)}))
    apply(line_from_A_to_B, 1, .norm_Eucl_vec)
    plot(theta, apply(line_from_A_to_B, 1, function(x){.distance_S_great_circle(line_from_A_to_B[1,], x)}))

    .plot_sphere()
    .plot_path_on_sphere(line_from_A_to_B)
    .plot_point_on_sphere(A, "red")
    .plot_point_on_sphere(B, "blue")

    Lambda = c(crossprod(A, B))
    B_prim = .deriv_rotated(A, B) # necessary to live on the tangent space then
    # .plot_point_on_sphere(B_prim, "green") # not nice because at distance 1 on the sphere.

    Tangent = list()
    t_vec = seq(from = 0.1, to = 1, by = 0.05)
    for(i in 1:length(t_vec)) {
      Tangent[[i]] = A + t_vec[i] * B_prim
      .plot_point_on_sphere(Tangent[[i]], "orange", 0.03)
    }
    # Length of segment in the tangent space (length of orange segment)
    print(paste0("Length of orange segment is ", round(.norm_Eucl_vec(B_prim), 8)))
  }

  print("Print three couple of points with tangent of unit length from red to blue")
  test_tangent_on_sphere(1)
  test_tangent_on_sphere(2)
  test_tangent_on_sphere(4)
}

##
# Test of samples on the sphere
##
visual_test_3 = function() {
  # check uniformity on S^2
  my_matrix = sample_on_S(n_elem = 100, dim_S = 2, seed = 1234)
  .plot_all_points_on_sphere(1, my_matrix, radius = 0.05, "black", "black")
  # check regularity on S^1
  elems = unif_on_S1(n_elem = 7)
  plot(elems, asp = 1)
  # check tetrahedron
  .plot_all_points_on_sphere(1, tetrahedron_on_S2(), 0.2, "red", "red")
  # check square on S1
  plot(square_on_S(1), asp = 1)
  # check square on S2 (one point is hidden by the tetrahedron)
  .plot_all_points_on_sphere(1, square_on_S(2), radius = 0.1, "cyan", "cyan")
}

##
# Test of push
##
visual_test_4 = function() {
  n_elem = 3
  seed = 1
  my_matrix = sample_on_S(n_elem = n_elem, dim_S = 2, seed = seed)
  # whether ascent and descent on the global mixture function
  # all types are -1 : attract particles together with -sin(x) = gradient ascent
  # all types are +1 : repulse particles with -sin(x) = gradient descent
  types = rep(+1, nrow(my_matrix)) #c(+1, -1, -1, -1)

  densitypes = rep(+1, nrow(my_matrix)) # -1 will give you negative density for this particle

  ## Change points
  #A = c(0, 1, 0)
  #B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
  #C = c(0.5, 0.9, 0); C = C / sqrt(sum(C^2))
  A = c(0, 1, 0)
  B = c(0, 0.9, 0.5); B = B / sqrt(sum(B^2))
  C = c(1, 0, 0)
  my_matrix[1,] = A
  my_matrix[2,] = B
  my_matrix[3,] = C
  ## End custom changes

  my_matrix_list = list()
  N = 50
  my_matrix_list[[1]] = my_matrix
  for(k in 2:N) {
    my_matrix_pushed = push(my_matrix_list[[k-1]], g_sin, densitypes, types, alpha = 0.33)
    my_matrix_list[[k]] = my_matrix_pushed
  }

  i = 1
  .plot_sphere()
  for(k in 1:N) {
    print(k)
    .plot_all_points_on_sphere(i, my_matrix_list[[k]])
  }
}

visual_test_5 = function() {
  # Helper
  push_with_plots = function(seed = 0, types = c(1, 1, 1), densitypes = c(1, 1, 1), alpha = 1) {
    my_matrix = sample_on_S(n_elem = 3, dim_S = 2, seed = seed)
    # types = rep(+1, nrow(my_matrix)) # whether ascent and descent on the global mixture function
    # densitypes = rep(+1, nrow(my_matrix)) # whether common and anti density
    # alpha = 1

    if(seed == 0) {
      ## Custom change 0
      A = c(0, 1, 0)
      B = .normalize_me_on_S(c(0, 0.9, 0.5))
      C = c(1, 0, 0)
      my_matrix[1,] = A
      my_matrix[2,] = B
      my_matrix[3,] = C
    } else if(seed == 1) {
      ## Custom change 1
      A = c(0, 1, 0)
      B = c(0, 0, 1)
      C = c(1, 0, 0)
      my_matrix[1,] = A
      my_matrix[2,] = B
      my_matrix[3,] = C
    }

    A = my_matrix[1,]
    B = my_matrix[2,]
    C = my_matrix[3,]
    colA = "red"
    colB = "blue"
    colC = "green"

    .plot_sphere()
    .plot_point_on_sphere(A, colA)
    .plot_point_on_sphere(B, colB)
    .plot_point_on_sphere(C, colC)

    # row = point 0 of the tangent space
    # col = outside point as seen on the tangent
    M1 = .matrix1_of_weighted_contribution(my_matrix, g_sin)
    Bprim_onA = M1[1, 2,]
    Cprim_onA = M1[1, 3,]
    Aprim_onB = M1[2, 1,]
    Cprim_onB = M1[2, 3,]
    Aprim_onC = M1[3, 1,]
    Bprim_onC = M1[3, 2,]

    .distance_S_great_circle(A, C)
    .distance_S_great_circle(B, C)

    length.out = 100

    .plot_segment_R_n(A, A + Bprim_onA, col = colB, length.out = length.out)
    .plot_segment_R_n(A, A + Cprim_onA, col = colC, length.out = length.out)
    .plot_segment_R_n(B, B + Aprim_onB, col = colA, length.out = length.out)
    .plot_segment_R_n(B, B + Cprim_onB, col = colC, length.out = length.out)
    .plot_segment_R_n(C, C + Aprim_onC, col = colA, length.out = length.out)
    .plot_segment_R_n(C, C + Bprim_onC, col = colB, length.out = length.out)
    .plot_segment_S_n(A, B, length.out = length.out)
    .plot_segment_S_n(A, C, length.out = length.out)
    .plot_segment_S_n(B, C, length.out = length.out)

    densitypes = c(1, 1, -1)
    M2 = .matrix2_of_weighted_contribution_with_densitypes(M1, densitypes)
    Bprim_onA = M2[1, 2,]
    Cprim_onA = M2[1, 3,]
    Aprim_onB = M2[2, 1,]
    Cprim_onB = M2[2, 3,]
    Aprim_onC = M2[3, 1,]
    Bprim_onC = M2[3, 2,]
    .plot_sphere()
    .plot_point_on_sphere(A, colA)
    .plot_point_on_sphere(B, colB)
    .plot_point_on_sphere(C, colC)
    .plot_segment_R_n(A, A + Bprim_onA, col = colB, length.out = length.out)
    .plot_segment_R_n(A, A + Cprim_onA, col = colC, length.out = length.out)
    .plot_segment_R_n(B, B + Aprim_onB, col = colA, length.out = length.out)
    .plot_segment_R_n(B, B + Cprim_onB, col = colC, length.out = length.out)
    .plot_segment_R_n(C, C + Aprim_onC, col = colA, length.out = length.out)
    .plot_segment_R_n(C, C + Bprim_onC, col = colB, length.out = length.out)
    .plot_segment_S_n(A, B, length.out = length.out)
    .plot_segment_S_n(A, C, length.out = length.out)
    .plot_segment_S_n(B, C, length.out = length.out)

    M3 = .matrix3_of_mean_action(M2)
    Bprim_onA = M2[1, 2,]
    Cprim_onA = M2[1, 3,]
    Aprim_onB = M2[2, 1,]
    Cprim_onB = M2[2, 3,]
    Aprim_onC = M2[3, 1,]
    Bprim_onC = M2[3, 2,]
    mean_onA = M3[1,]
    mean_onB = M3[2,]
    mean_onC = M3[3,]
    .plot_sphere()
    .plot_point_on_sphere(A, colA)
    .plot_point_on_sphere(B, colB)
    .plot_point_on_sphere(C, colC)
    .plot_segment_R_n(A, A + Bprim_onA, col = colB, length.out = length.out)
    .plot_segment_R_n(A, A + Cprim_onA, col = colC, length.out = length.out)
    .plot_segment_R_n(B, B + Aprim_onB, col = colA, length.out = length.out)
    .plot_segment_R_n(B, B + Cprim_onB, col = colC, length.out = length.out)
    .plot_segment_R_n(C, C + Aprim_onC, col = colA, length.out = length.out)
    .plot_segment_R_n(C, C + Bprim_onC, col = colB, length.out = length.out)
    .plot_segment_R_n(A, A + mean_onA, col = "goldenrod", length.out = length.out)
    .plot_segment_R_n(B, B + mean_onB, col = "goldenrod", length.out = length.out)
    .plot_segment_R_n(C, C + mean_onC, col = "goldenrod", length.out = length.out)
    .plot_segment_S_n(A, B, length.out = length.out)
    .plot_segment_S_n(A, C, length.out = length.out)
    .plot_segment_S_n(B, C, length.out = length.out)

    types = c(1,1,-1)
    M4 = .matrix4_of_mean_action_with_types(M3, types, alpha)
    Bprim_onA = M2[1, 2,]
    Cprim_onA = M2[1, 3,]
    Aprim_onB = M2[2, 1,]
    Cprim_onB = M2[2, 3,]
    Aprim_onC = M2[3, 1,]
    Bprim_onC = M2[3, 2,]
    mean_onA = M4[1,]
    mean_onB = M4[2,]
    mean_onC = M4[3,]
    .plot_sphere()
    .plot_point_on_sphere(A, colA)
    .plot_point_on_sphere(B, colB)
    .plot_point_on_sphere(C, colC)
    .plot_segment_R_n(A, A + Bprim_onA, col = colB, length.out = length.out)
    .plot_segment_R_n(A, A + Cprim_onA, col = colC, length.out = length.out)
    .plot_segment_R_n(B, B + Aprim_onB, col = colA, length.out = length.out)
    .plot_segment_R_n(B, B + Cprim_onB, col = colC, length.out = length.out)
    .plot_segment_R_n(C, C + Aprim_onC, col = colA, length.out = length.out)
    .plot_segment_R_n(C, C + Bprim_onC, col = colB, length.out = length.out)
    .plot_segment_R_n(A, A + mean_onA, col = "darkgoldenrod", length.out = length.out)
    .plot_segment_R_n(B, B + mean_onB, col = "darkgoldenrod", length.out = length.out)
    .plot_segment_R_n(C, C + mean_onC, col = "darkgoldenrod", length.out = length.out)
    .plot_segment_S_n(A, B, length.out = length.out)
    .plot_segment_S_n(A, C, length.out = length.out)
    .plot_segment_S_n(B, C, length.out = length.out)

    my_matrix_pushed = .matrix5_of_mean_actions_with_types_on_original_space(my_matrix, M4)
    mean_onA = M4[1,]
    mean_onB = M4[2,]
    mean_onC = M4[3,]
    mean_onA_sphere = my_matrix_pushed[1,]
    mean_onB_sphere = my_matrix_pushed[2,]
    mean_onC_sphere = my_matrix_pushed[3,]
    .plot_sphere()
    .plot_point_on_sphere(A, colA)
    .plot_point_on_sphere(B, colB)
    .plot_point_on_sphere(C, colC)
    .plot_segment_R_n(A, A + mean_onA, col = colA, length.out = length.out)
    .plot_segment_R_n(B, B + mean_onB, col = colB, length.out = length.out)
    .plot_segment_R_n(C, C + mean_onC, col = colC, length.out = length.out)
    .plot_segment_S_n(A, mean_onA_sphere, col = paste0("dark", colA), length.out = length.out)
    .plot_segment_S_n(B, mean_onB_sphere, col = paste0("dark", colB), length.out = length.out)
    .plot_segment_S_n(C, mean_onC_sphere, col = paste0("dark", colC), length.out = length.out)
    .plot_segment_S_n(A, B, length.out = length.out)
    .plot_segment_S_n(A, C, length.out = length.out)
    .plot_segment_S_n(B, C, length.out = length.out)
    .plot_point_on_sphere(mean_onA_sphere, paste0("dark", colA))
    .plot_point_on_sphere(mean_onB_sphere, paste0("dark", colB))
    .plot_point_on_sphere(mean_onC_sphere, paste0("dark", colC))
  }

  push_with_plots()
}

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

####################################
# Plotting functions on the sphere #
####################################
.plot_sphere = function() {
  # https://stackoverflow.com/questions/34539268
  rgl::spheres3d(0, 0, 0, lit = FALSE, color = "white")
  rgl::spheres3d(0, 0, 0, radius = 1.01, lit = FALSE, color = "black", front = "lines")
}

.plot_path_on_sphere = function(traj, col = "black", radius = 0.02) {
  # traj is a general trajectory
  x <- traj[,1]
  y <- traj[,2]
  z <- traj[,3]
  rgl::spheres3d(x, y, z, col = col, radius = radius)
}

.plot_point_on_sphere = function(A, col = "red", radius = 0.1) {
  rgl::spheres3d(A[1], A[2], A[3], col = col, radius = radius)
}

.plot_all_points_on_sphere = function(i, my_matrix, radius = 0.05,
                                     col_i = "red",
                                     col_others = "black") {
  .plot_point_on_sphere(my_matrix[i,], col_i, radius)
  for(j in (1:nrow(my_matrix))[-i]) {
    .plot_point_on_sphere(my_matrix[j,], col_others, radius)
  }
}

.plot_segment_R_n = function(A, B, col = "black", length.out = 100) {
  segment_R_n = .segment_R_n_func(A, B, length.out)
  .plot_path_on_sphere(segment_R_n, col = col)
}

.segment_R_n_func = function(A, B, length.out = 100) {
  # on the tangent space in R^{n-1}. Not ok for segment in S^{n-1}
  theta = seq(from = 0, to = 1, length.out = length.out)
  segment_R_n = t(sapply(theta, function(t) {t * A + (1-t) * B}))
  return(segment_R_n)
}

.plot_segment_S_n = function(A, B, col = "black", length.out = 100) {
  segment_S_n = .segment_S_n_func(A, B, length.out)
  .plot_path_on_sphere(segment_S_n, col = col)
}

.segment_S_n_func = function(A, B, t_max = "segment", length.out = 100) {
  if(t_max == "line") {
    t_max = 2*pi
  } else if(t_max == "segment") {
    t_max = .distance_S_great_circle(A, B)
  } else if(t_max == "semisegment") {
    t_max = .distance_S_great_circle(A, B) / 2
  }
  theta = seq(from = 0, to = t_max, length.out = length.out)
  line_from_A_to_B = t(sapply(theta, function(t) {.rotated(A, B, t)}))
  return(line_from_A_to_B)
}
