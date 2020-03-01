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
#
##
