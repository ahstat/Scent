test_that("g functions are 0 in 0", {
  g1 = g_sin
  g2 = g_sinc
  g3 = g_sin_alpha(2)
  g4 = g_sin_alpha(1/2)
  g5 = g_ddnorm(1, 1)
  g6 = g_ddnorm(2, 1)
  g7 = g_ddnorm(3, 1)
  g8 = g_ddnorm(4, 5)
  expect_equal(g1(0), 0)
  expect_equal(g2(0), 0)
  expect_equal(g3(0), 0)
  expect_equal(g4(0), 0)
  expect_equal(g5(0), 0)
  expect_equal(g6(0), 0)
  expect_equal(g7(0), 0)
  expect_equal(g8(0), 0)
})

test_that("g_ddnorm sums to 1 in dimension 1", {
  g1 = g_ddnorm(1, 1)
  g2 = g_ddnorm(1, 1/2)
  g3 = g_ddnorm(1, 5)
  step = 0.01
  x = seq(-10, 10, by = step)
  # Integral on R of the primitive of gi
  expect_equal(sum(cumsum(g1(x)*step)*step), 1)
  expect_equal(sum(cumsum(g2(x)*step)*step), 1)
  x = seq(-100, 100, by = step) # more variance so need larger x
  expect_equal(sum(cumsum(g3(x)*step)*step), 1)

  # Not tested in other dimensions
})
