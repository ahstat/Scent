test_that(".rotated_old and .rotated give same result", {
  A = c(1, 0, 0, -1, -1)
  B = c(1, 2, 3, 4, 5)
  A = .normalize_me_on_S(A) # be sure A is on the sphere!
  B = .normalize_me_on_S(B) # be sure B is on the sphere!
  theta = seq(from = 0, to = 2*pi, length.out = 100)
  line_from_A_to_A_by_B = t(sapply(theta, function(t) {.rotated(A, B, t)}))
  line_from_A_to_A_by_B_old = t(sapply(theta, function(t) {.rotated_old(A, B, t)}))
  expect_equal(line_from_A_to_A_by_B,
               line_from_A_to_A_by_B_old)
})

test_that(".deriv_rotated_old and .deriv_rotated give same result", {
  A = c(1, 0, 0, -1, -1)
  B = c(1, 2, 3, 4, 5)
  A = .normalize_me_on_S(A) # be sure A is on the sphere!
  B = .normalize_me_on_S(B) # be sure B is on the sphere!
  expect_equal(.deriv_rotated_old(A, B),
               .deriv_rotated(A, B))
})

test_that(".rotated for distance 0, pi/2, pi, 3*pi/2, 2*pi give special known values", {
  A = c(1, 0, 0, -1, -1)
  B = c(1, 2, 3, 4, 5)
  A = .normalize_me_on_S(A) # be sure A is on the sphere!
  B = .normalize_me_on_S(B) # be sure B is on the sphere!
  # Tests of:
  # .rotated(A, B, 0) gives A
  # .rotated(A, B, pi/2) gives B_prim (such that <A|B_prim>=0 and A,B,B_prim on the same circle)
  # .rotated(A, B, pi) gives -A
  # .rotated(A, B, 3*pi/2) gives -B_prim
  A = .normalize_me_on_S(A) # be sure A is on the sphere!
  B = .normalize_me_on_S(B) # be sure B is on the sphere!
  expect_equal(.rotated(A, B, 0),
               A)
  expect_equal(.rotated(A, B, pi/2),
               -.rotated(A, B, 3*pi/2))
  expect_equal(.rotated(A, B, pi),
               -A)
  expect_equal(.rotated(A, B, 2*pi),
               A)
})

test_that(".rotated outputs on the sphere", {
  my_matrix = sample_on_S(n_elem = 2, dim_S = 2, seed = 1234)
  A = my_matrix[1,]
  B = my_matrix[2,]
  theta = seq(from = 0, to = 2*pi, length.out = 100)
  line_from_A_to_A_by_B = t(sapply(theta, function(t) {.rotated(A, B, t)}))
  expect_equal(apply(line_from_A_to_A_by_B, 1, .norm_Eucl_vec),
               rep(1,100))
})

# Around 30 seconds for testing
# test_that("visual tests do not throw error", {
#   expect_error(visual_test_1(), NA)
#   expect_error(visual_test_2(), NA)
#   expect_error(visual_test_3(), NA)
#   expect_error(visual_test_4(), NA)
#   expect_error(visual_test_5(), NA)
# })
