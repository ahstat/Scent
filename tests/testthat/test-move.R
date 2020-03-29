test_that("Push one step in Euclidian case (test 1 without Log/Exp)", {
  my_matrix = matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)
  g = g_sin
  densitypes = c(1, 1)
  types = c(1, 1)
  alpha = 1
  manifold = "E"
  expect_equal(push(my_matrix, g, densitypes, types, alpha, manifold),
               .test_move_Euclidian(my_matrix, g, densitypes, types, alpha))
})

test_that("Push one step in Euclidian case (test 2 without Log/Exp)", {
  set.seed(2713)
  dim_E = 5
  n_elem = 20
  my_matrix = matrix(runif(dim_E*n_elem, -10, 10), ncol = dim_E, byrow = TRUE)
  g = g_sin
  densitypes = rspin(n_elem)
  types = rspin(n_elem)
  alpha = 0.03
  manifold = "E"
  expect_equal(push(my_matrix, g, densitypes, types, alpha, manifold),
               .test_move_Euclidian(my_matrix, g, densitypes, types, alpha))
})

test_that("Push one step in Euclidian case (test 1 with Log/Exp)", {
  my_matrix = matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)
  g = g_sin
  densitypes = c(1, 1)
  types = c(1, 1)
  alpha = 1
  manifold = "E"
  expect_equal(push(my_matrix, g, densitypes, types, alpha, manifold),
               .test_move_Manifold(my_matrix, g, densitypes, types, alpha, manifold))
})

test_that("Push one step in Euclidian case (test 2 with Log/Exp)", {
  set.seed(2713)
  dim_E = 5
  n_elem = 20
  my_matrix = matrix(runif(dim_E*n_elem, -10, 10), ncol = dim_E, byrow = TRUE)
  g = g_sin
  densitypes = rspin(n_elem)
  types = rspin(n_elem)
  alpha = 0.03
  manifold = "E"
  expect_equal(push(my_matrix, g, densitypes, types, alpha, manifold),
               .test_move_Manifold(my_matrix, g, densitypes, types, alpha, manifold))
})

test_that("Push one step in Spherical case (test 1 with Log/Exp)", {
  my_matrix = unif_on_S1(3)
  g = g_sin
  densitypes = c(1, 1, -1)
  types = c(1, -1, -1)
  alpha = 1
  manifold = "S"
  expect_equal(push(my_matrix, g, densitypes, types, alpha, manifold),
               .test_move_Manifold(my_matrix, g, densitypes, types, alpha, manifold))
})

test_that("Push one step in Spherical case (test 2 with Log/Exp)", {
  seed = 2713
  dim_S = 5
  n_elem = 20
  my_matrix = sample_on_S(n_elem, dim_S, seed)
  g = g_sin
  densitypes = rspin(n_elem)
  types = rspin(n_elem)
  alpha = 0.03
  manifold = "S"
  expect_equal(push(my_matrix, g, densitypes, types, alpha, manifold),
               .test_move_Manifold(my_matrix, g, densitypes, types, alpha, manifold))
})

test_that("Check dimensions in get_evol", {
  my_matrix = unif_on_S1(3)
  g = g_sin
  densitypes = c(1, 1, -1)
  types = c(1, -1, -1)
  alpha = 1
  manifold = "S"
  out_1 = get_evol(N = 1, my_matrix, g, densitypes, types, alpha, manifold)
  out_20 = get_evol(N = 20, my_matrix, g, densitypes, types, alpha, manifold)
  expect_equal(dim(out_1),
               c(3, 2, 1))
  expect_equal(dim(out_20),
               c(3, 2, 20))
})
