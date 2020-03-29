test_that("rspin outputs -1 or 1", {
  set.seed(2713)
  expect_equal(all(rspin(1000) %in% c(-1, 1)),
               TRUE)
})

test_that("sample_on_S outputs on the sphere", {
  my_matrix = sample_on_S(n_elem = 100, dim_S = 4, seed = 1234)
  expect_equal(apply(my_matrix, 1, .norm_Eucl_vec),
               rep(1, 100))
})

test_that("unif_on_S1 outputs on the sphere", {
  my_matrix = sapply(1:20, unif_on_S1)
  expect_equal(unlist(lapply(my_matrix, function(l) {apply(l, 1, .norm_Eucl_vec)})),
               rep(1, sum(1:20)))
})

test_that("tetrahedron_on_S2 outputs on the sphere", {
  my_matrix = tetrahedron_on_S2()
  expect_equal(apply(my_matrix, 1, .norm_Eucl_vec),
               rep(1, 4))
})

test_that("square_on_S outputs on the sphere", {
  my_matrix = sapply(1:5, square_on_S)
  expect_equal(unlist(lapply(my_matrix, function(l) {apply(l, 1, .norm_Eucl_vec)})),
               rep(1, sum(unlist(lapply(my_matrix, nrow)))))
})

test_that("icositetrachore_on_S3 outputs on the sphere", {
  my_matrix = icositetrachore_on_S3()
  expect_equal(apply(my_matrix, 1, .norm_Eucl_vec),
               rep(1, 24))
})
