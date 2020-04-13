test_that("get_Tmax, get_N, get_alpha are correctly linked", {
  N = c(1, 3, 10, 100, 300, 1000, 3000, 10000, 30000, 100000)
  alpha = c(0.0001, 0.0003, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30)
  df = expand.grid(N = N, alpha = alpha)
  df$Tmax = df$N * df$alpha
  expect_equal(get_Tmax(df$alpha, df$N), df$Tmax)
  expect_equal(get_N(df$alpha, df$Tmax), df$N) # for reference only, output may not be exact integer
  expect_equal(get_alpha(df$N, df$Tmax), df$alpha)
})

test_that("diffposition gives the distance between positions for the manifold E", {
  my_matrix = unif_on_S1(3)
  g = g_sin
  densitypes = c(1, 1, -exp(1i * pi/4))
  #densitypes = c(1, 1, -1)
  types = c(-exp(1i * pi/4), -1, -1)
  manifold = "E"

  alpha = 1
  N = 100
  Evolution = get_evol(N = N, my_matrix, g, densitypes, types, alpha, manifold)
  out = diffposition_func(Evolution, manifold)

  # For first element, it is a equilateral triangle on the unit circle, so
  # all particles at same distance of sqrt(3) (Euclidian distance)
  M = matrix(sqrt(3), ncol = 3, nrow = 3)
  colnames(M) = paste0("part", 1:3)
  rownames(M) = colnames(M)
  diag(M) = 0
  expect_equal(out[,,1], M)

  # For the 100th element, all points close to the same position,
  # so distance should be close to 0 (Euclidian distance)
  M = matrix(0, ncol = 3, nrow = 3)
  colnames(M) = paste0("part", 1:3)
  rownames(M) = colnames(M)
  expect_equal(out[,,N], M)
})

test_that("diffposition gives the distance between positions for the manifold S", {
  my_matrix = unif_on_S1(3)
  g = g_sin
  densitypes = c(1, 1, -exp(1i * pi/4))
  #densitypes = c(1, 1, -1)
  types = c(-exp(1i * pi/4), -1, -1)
  manifold = "S"

  alpha = 1
  N = 100
  Evolution = get_evol(N = N, my_matrix, g, densitypes, types, alpha, manifold)
  out = diffposition_func(Evolution, manifold)

  # For first element, it is a equilateral triangle on the unit circle, so
  # all particles at same distance of 2*pi/3 (distance on the circle)
  M = matrix(2*pi/3, ncol = 3, nrow = 3)
  colnames(M) = paste0("part", 1:3)
  rownames(M) = colnames(M)
  diag(M) = 0
  expect_equal(out[,,1], M)

  # For the 100th element, all points close to 0,
  # so distance should be close to 0 (Euclidian distance)
  M = matrix(0, ncol = 3, nrow = 3)
  colnames(M) = paste0("part", 1:3)
  rownames(M) = colnames(M)
  expect_equal(out[,,N], M)
})

test_that("velocity converges when alpha --> 0 for manifold E", {
  my_matrix = unif_on_S1(3)
  g = g_sin
  densitypes = c(1, 1, -exp(1i * pi/4))
  #densitypes = c(1, 1, -1)
  types = c(-exp(1i * pi/4), -1, -1)
  manifold = "E"

  Tmax = 10

  alpha1 = 0.1
  N = get_N(alpha1, Tmax)
  Evolution1 = get_evol(N, my_matrix, g, densitypes, types, alpha1, manifold)

  alpha2 = 0.01
  N = get_N(alpha2, Tmax)
  Evolution2 = get_evol(N, my_matrix, g, densitypes, types, alpha2, manifold)

  speed1 = velocity_func(Evolution1, manifold, alpha1)[,1]
  speed2 = velocity_func(Evolution2, manifold, alpha2)[,1]
  speed2_fewpoints = speed2[seq(from = 1, to = 1000, by = 10)]
  expect_equal(speed1, speed2_fewpoints, tolerance = 1e-2)

  # plot(speed1, speed2_fewpoints)
  # abline(a=0,b=1, col = "red")

  # # Additional tests (quite long for running in routine)
  # alpha3 = 0.001
  # N = get_N(alpha3, Tmax)
  # Evolution3 = get_evol(N, my_matrix, g, densitypes, types, alpha3, manifold)
  # speed3 = velocity_func(Evolution3, manifold, alpha3)[,1]
  # speed3_fewpoints = speed3[seq(from = 1, to = 10000, by = 10)]
  # expect_equal(speed2, speed3_fewpoints, tolerance = 1e-3)
  #
  # alpha4 = 0.0001
  # N = get_N(alpha4, Tmax)
  # Evolution4 = get_evol(N, my_matrix, g, densitypes, types, alpha4, manifold) # < 1 minute
  # speed4 = velocity_func(Evolution4, manifold, alpha4)[,1]
  # speed4_fewpoints = speed4[seq(from = 1, to = 100000, by = 10)]
  # expect_equal(speed3, speed4_fewpoints, tolerance = 1e-4)
})

test_that("velocity converges when alpha --> 0 for manifold S", {
  my_matrix = unif_on_S1(3)
  g = g_sin
  densitypes = c(1, 1, -exp(1i * pi/4))
  #densitypes = c(1, 1, -1)
  types = c(-exp(1i * pi/4), -1, -1)
  manifold = "S"

  Tmax = 10

  alpha1 = 0.1
  N = get_N(alpha1, Tmax)
  Evolution1 = get_evol(N, my_matrix, g, densitypes, types, alpha1, manifold)

  alpha2 = 0.01
  N = get_N(alpha2, Tmax)
  Evolution2 = get_evol(N, my_matrix, g, densitypes, types, alpha2, manifold)

  speed1 = velocity_func(Evolution1, manifold, alpha1)[,1]
  speed2 = velocity_func(Evolution2, manifold, alpha2)[,1]
  speed2_fewpoints = speed2[seq(from = 1, to = 1000, by = 10)]
  expect_equal(speed1, speed2_fewpoints, tolerance = 2*1e-2)
})

# TODO: more velocity tests
# TODO: acceleration tests
# TODO: summary_func tests
