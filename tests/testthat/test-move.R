test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("ddd works", {
  expect_equal(.distance_E(1, 3), 2)
})

test_that("eee works", {
  expect_equal(as.numeric(scent:::.matrix1_of_weighted_contribution(matrix(1), function(x){-sin(x)})),
               0)
})
