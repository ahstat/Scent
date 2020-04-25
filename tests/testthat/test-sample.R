test_that("pseudo_random_number gives different number each time", {
  a = pseudo_random_number()
  Sys.sleep(0.02)
  b = pseudo_random_number()
  expect_equal(a != b,
               TRUE)
})

test_that("circle for 2 outputs 1 -1", {
  expect_equal(circle_elements(2),
               c(1, -1))
})

test_that("circle for 4 outputs 1 1i -1 -1i", {
  expect_equal(circle_elements(4),
               c(1, 1i, -1, -1i))
})

test_that("rspin with nb_on_circle to 2 outputs -1, 1", {
  set.seed(2713)
  expect_equal(all(rspin(1000, 2) %in% c(-1, 1)),
               TRUE)
})

test_that("rspin with nb_on_circle to 4 outputs -1, 1, i, -i", {
  set.seed(2713)
  expect_equal(all(rspin(1000, 4) %in% c(-1, 1, -1i, 1i)),
               TRUE)
})

test_that("combin with many elements still gives typeX and densitypeX colnames (with 'type' only)", {
  n_elem = 12
  nb_on_circle = 2
  out = combin(n_elem, nb_on_circle)
  expect_equal(length(out), 2^12)
  expect_equal(out[[1]], rep(1, n_elem))
  expect_equal(out[[4096]], rep(-1, n_elem))
})

test_that("combin with nb_on_circle = 3 gives elements on circle_elements(3)", {
  n_elem = 5
  nb_on_circle = 3
  out = combin(n_elem, nb_on_circle)
  expect_equal(unique(unlist(out)) ,
               circle_elements(3))
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
