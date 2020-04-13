#############
# Euclidian #
#############

test_that(".norm_Eucl_vec gives norm in dimension 1", {
  elems = -5:5
  expect_equal(sapply(elems, .norm_Eucl_vec),
               abs(elems))
})

test_that(".norm_Eucl_vec gives norm in dimension n", {
  elem = c(1, 0, 0, -1, -1)
  expect_equal(.norm_Eucl_vec(elem),
               sqrt(3))
})

test_that(".norm_Eucl_vec gives norm in dimension n for complex numbers", {
  elem = c(1+1i, 2-3*1i, -5-4*1i)
  expect_equal(.norm_Eucl_vec(elem),
               sqrt(sum(abs(elem)^2)))
})

test_that(".distance_E gives distance in dimension n", {
  A = c(1, 0, 0, -1, -1)
  B = c(1, 1, 1, 1, 1)
  expect_equal(.distance_E(A, B),
               sqrt(1+1+2^2+2^2))
})

test_that("Exp(Log(x)) is x for Euclidian", {
  A = c(1, 0, 0, -1, -1)
  B = c(1, 2, 3, 4, 5)
  g = function(x) {x}
  B_on_tanA = .Log_weighted_E(A, B, g)
  B_back = .Exp_E(A, B_on_tanA)
  expect_equal(B_back,
               B)
})

test_that("Log_x(x)) is 0 for Euclidian", {
  A = c(1, 0, 0, -1, -1)
  B = c(1, 2, 3, 4, 5)
  g = function(x) {-sin(x)}
  A_on_tanA = .Log_weighted_E(A, A, g)
  B_on_tanB = .Log_weighted_E(B, B, g)
  expect_equal(A_on_tanA, rep(0, 5))
  expect_equal(B_on_tanB, rep(0, 5))
})

#############
# Spherical #
#############

test_that(".normalize_me_on_S gives element of S for non zero elements", {
  elem1 = c(1, 2, 3)
  set.seed(2713)
  elem2 = runif(10)
  elem3 = c(-1, 0, 0, 0)
  expect_equal(sum(.normalize_me_on_S(elem1)^2),
               1)
  expect_equal(sum(.normalize_me_on_S(elem2)^2),
               1)
  expect_equal(sum(.normalize_me_on_S(elem3)^2),
               1)
})

test_that(".normalize_me_on_S gives element same element if element is on S", {
  # Check that p o p = p
  elem1 = c(1, 2, 3)
  set.seed(2713)
  elem2 = runif(10)
  elem3 = c(-1, 0, 0, 0)
  expect_equal(.normalize_me_on_S(.normalize_me_on_S(elem1)),
               .normalize_me_on_S(elem1))
  expect_equal(.normalize_me_on_S(.normalize_me_on_S(elem2)),
               .normalize_me_on_S(elem2))
  expect_equal(.normalize_me_on_S(elem3),
               elem3) # once for elem3, since elem3 already in S
})

test_that(".normalize_me_on_S gives error for 0 element", {
  elem = rep(0, 5)
  expect_error(.normalize_me_on_S(elem))
})

test_that(".distance_S_great_circle for S^1", {
  # S^1, so each element is represented with 2 directions
  # Distance between 0 and 1 on the circle is pi/2
  A = c(1, 0)
  B = c(0, 1)
  expect_equal(.distance_S_great_circle(A, B),
               pi/2)
  expect_equal(.distance_S_great_circle(B, A),
               pi/2) # should be a distance, so >= 0 in all cases
})

test_that(".distance_S_great_circle for S^1 for same elements", {
  # S^1, so each element is represented with 2 directions
  # Distance between A and A on the circle is 0
  A = c(1, 0)
  B = c(0, 1)
  expect_equal(.distance_S_great_circle(A, A),
               0)
  expect_equal(.distance_S_great_circle(B, B),
               0)
})

test_that(".distance_S_great_circle for S^1 for antipodal elements", {
  # S^1, so each element is represented with 2 directions
  # Distance between B and B on the circle is pi
  A = c(1, 0)
  B = c(0, -1)
  expect_equal(.distance_S_great_circle(A, -A),
               pi)
  expect_equal(.distance_S_great_circle(B, -B),
               pi)
})

test_that(".distance_S_great_circle for S^2", {
  # S^2, so each element is represented with 3 directions
  # Distance between 0 and 1 on the circle is pi/2
  A = c(1, 0, 0)
  B = c(0, 0, -1)
  expect_equal(.distance_S_great_circle(A, B),
               pi/2)
  expect_equal(.distance_S_great_circle(B, A),
               pi/2)
  expect_equal(.distance_S_great_circle(A, A),
               0)
  expect_equal(.distance_S_great_circle(B, B),
               0)
  expect_equal(.distance_S_great_circle(A, -A),
               pi)
  expect_equal(.distance_S_great_circle(B, -B),
               pi)
})

test_that("Exp(Log(x)) is x for Spherical", {
  A = c(1, 0, 0, -1, -1)
  B = c(1, 2, 3, 4, 5)
  A = .normalize_me_on_S(A) # be sure A is on the sphere!
  B = .normalize_me_on_S(B) # be sure B is on the sphere!
  g = function(x) {x}
  B_on_tanA = .Log_weighted_S(A, B, g)
  B_back = .Exp_S(A, B_on_tanA)
  expect_equal(B_back,
               B)
})

test_that("Log_x(x)) is 0 for Spherical", {
  A = c(1, 0, 0, -1, -1)
  B = c(1, 2, 3, 4, 5)
  A = .normalize_me_on_S(A) # be sure A is on the sphere!
  B = .normalize_me_on_S(B) # be sure B is on the sphere!
  g = function(x) {-sin(x)}
  A_on_tanA = .Log_weighted_S(A, A, g)
  B_on_tanB = .Log_weighted_S(B, B, g)
  expect_equal(A_on_tanA, rep(0, 5))
  expect_equal(B_on_tanB, rep(0, 5))
})

##############
# Hyperbolic #
##############

# Unit tests not done (yet, or never)
