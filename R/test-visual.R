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

    plot_sphere()
    plot_path_on_sphere(line_from_A_to_B, col = "black")
    plot_point_on_sphere(A, "red")
    plot_point_on_sphere(B, "blue")
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

    plot_sphere()
    plot_path_on_sphere(line_from_A_to_B)
    plot_point_on_sphere(A, "red")
    plot_point_on_sphere(B, "blue")

    Lambda = c(crossprod(A, B))
    B_prim = .deriv_rotated(A, B) # necessary to live on the tangent space then
    # plot_point_on_sphere(B_prim, "green") # not nice because at distance 1 on the sphere.

    Tangent = list()
    t_vec = seq(from = 0.1, to = 1, by = 0.05)
    for(i in 1:length(t_vec)) {
      Tangent[[i]] = A + t_vec[i] * B_prim
      plot_point_on_sphere(Tangent[[i]], "orange", 0.03)
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
  plot_all_points_on_sphere(1, my_matrix, radius = 0.05, "black", "black")
  # check regularity on S^1
  elems = unif_on_S1(n_elem = 7)
  plot(elems, asp = 1)
  # check tetrahedron
  plot_all_points_on_sphere(1, tetrahedron_on_S2(), 0.2, "red", "red")
  # check square on S1
  plot(square_on_S(1), asp = 1)
  # check square on S2 (one point is hidden by the tetrahedron)
  plot_all_points_on_sphere(1, square_on_S(2), radius = 0.1, "cyan", "cyan")
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
    my_matrix_pushed = push(my_matrix_list[[k-1]], g, densitypes, types, alpha = 0.33)
    my_matrix_list[[k]] = my_matrix_pushed
  }

  i = 1
  plot_sphere()
  for(k in 1:N) {
    print(k)
    plot_all_points_on_sphere(i, my_matrix_list[[k]])
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

    plot_sphere()
    plot_point_on_sphere(A, colA)
    plot_point_on_sphere(B, colB)
    plot_point_on_sphere(C, colC)

    # row = point 0 of the tangent space
    # col = outside point as seen on the tangent
    M1 = .matrix1_of_weighted_contribution(my_matrix, g)
    Bprim_onA = M1[1, 2,]
    Cprim_onA = M1[1, 3,]
    Aprim_onB = M1[2, 1,]
    Cprim_onB = M1[2, 3,]
    Aprim_onC = M1[3, 1,]
    Bprim_onC = M1[3, 2,]

    .distance_S_great_circle(A, C)
    .distance_S_great_circle(B, C)

    plot_segment_R_n(A, A + Bprim_onA, col = colB)
    plot_segment_R_n(A, A + Cprim_onA, col = colC)
    plot_segment_R_n(B, B + Aprim_onB, col = colA)
    plot_segment_R_n(B, B + Cprim_onB, col = colC)
    plot_segment_R_n(C, C + Aprim_onC, col = colA)
    plot_segment_R_n(C, C + Bprim_onC, col = colB)
    plot_segment_S_n(A, B)
    plot_segment_S_n(A, C)
    plot_segment_S_n(B, C)

    densitypes = c(1, 1, -1)
    M2 = .matrix2_of_weighted_contribution_with_densitypes(M1, densitypes)
    Bprim_onA = M2[1, 2,]
    Cprim_onA = M2[1, 3,]
    Aprim_onB = M2[2, 1,]
    Cprim_onB = M2[2, 3,]
    Aprim_onC = M2[3, 1,]
    Bprim_onC = M2[3, 2,]
    plot_sphere()
    plot_point_on_sphere(A, colA)
    plot_point_on_sphere(B, colB)
    plot_point_on_sphere(C, colC)
    plot_segment_R_n(A, A + Bprim_onA, col = colB)
    plot_segment_R_n(A, A + Cprim_onA, col = colC)
    plot_segment_R_n(B, B + Aprim_onB, col = colA)
    plot_segment_R_n(B, B + Cprim_onB, col = colC)
    plot_segment_R_n(C, C + Aprim_onC, col = colA)
    plot_segment_R_n(C, C + Bprim_onC, col = colB)
    plot_segment_S_n(A, B)
    plot_segment_S_n(A, C)
    plot_segment_S_n(B, C)

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
    plot_sphere()
    plot_point_on_sphere(A, colA)
    plot_point_on_sphere(B, colB)
    plot_point_on_sphere(C, colC)
    plot_segment_R_n(A, A + Bprim_onA, col = colB)
    plot_segment_R_n(A, A + Cprim_onA, col = colC)
    plot_segment_R_n(B, B + Aprim_onB, col = colA)
    plot_segment_R_n(B, B + Cprim_onB, col = colC)
    plot_segment_R_n(C, C + Aprim_onC, col = colA)
    plot_segment_R_n(C, C + Bprim_onC, col = colB)
    plot_segment_R_n(A, A + mean_onA, col = "goldenrod")
    plot_segment_R_n(B, B + mean_onB, col = "goldenrod")
    plot_segment_R_n(C, C + mean_onC, col = "goldenrod")
    plot_segment_S_n(A, B)
    plot_segment_S_n(A, C)
    plot_segment_S_n(B, C)

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
    plot_sphere()
    plot_point_on_sphere(A, colA)
    plot_point_on_sphere(B, colB)
    plot_point_on_sphere(C, colC)
    plot_segment_R_n(A, A + Bprim_onA, col = colB)
    plot_segment_R_n(A, A + Cprim_onA, col = colC)
    plot_segment_R_n(B, B + Aprim_onB, col = colA)
    plot_segment_R_n(B, B + Cprim_onB, col = colC)
    plot_segment_R_n(C, C + Aprim_onC, col = colA)
    plot_segment_R_n(C, C + Bprim_onC, col = colB)
    plot_segment_R_n(A, A + mean_onA, col = "darkgoldenrod")
    plot_segment_R_n(B, B + mean_onB, col = "darkgoldenrod")
    plot_segment_R_n(C, C + mean_onC, col = "darkgoldenrod")
    plot_segment_S_n(A, B)
    plot_segment_S_n(A, C)
    plot_segment_S_n(B, C)

    my_matrix_pushed = .matrix5_of_mean_actions_with_types_on_original_space(my_matrix, M4)
    mean_onA = M4[1,]
    mean_onB = M4[2,]
    mean_onC = M4[3,]
    mean_onA_sphere = my_matrix_pushed[1,]
    mean_onB_sphere = my_matrix_pushed[2,]
    mean_onC_sphere = my_matrix_pushed[3,]
    plot_sphere()
    plot_point_on_sphere(A, colA)
    plot_point_on_sphere(B, colB)
    plot_point_on_sphere(C, colC)
    plot_segment_R_n(A, A + mean_onA, col = colA)
    plot_segment_R_n(B, B + mean_onB, col = colB)
    plot_segment_R_n(C, C + mean_onC, col = colC)
    plot_segment_S_n(A, mean_onA_sphere, col = paste0("dark", colA))
    plot_segment_S_n(B, mean_onB_sphere, col = paste0("dark", colB))
    plot_segment_S_n(C, mean_onC_sphere, col = paste0("dark", colC))
    plot_segment_S_n(A, B)
    plot_segment_S_n(A, C)
    plot_segment_S_n(B, C)
    plot_point_on_sphere(mean_onA_sphere, paste0("dark", colA))
    plot_point_on_sphere(mean_onB_sphere, paste0("dark", colB))
    plot_point_on_sphere(mean_onC_sphere, paste0("dark", colC))
  }

  push_with_plots()
}

