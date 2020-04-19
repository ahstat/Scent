
test6 = function(first_one = TRUE) {
  n_elem = 3
  dim_S = 1
  my_matrix = sample_on_S(n_elem, dim_S)
  A = c(Re(exp(1i*0)), Im(exp(1i*0)))
  B = c(Re(exp(1i*(2/3)*pi)), Im(exp(1i*(2/3)*pi)))
  C = c(Re(exp(-1i*(2/3)*pi)), Im(exp(-1i*(2/3)*pi)))
  my_matrix[1,] = A
  my_matrix[2,] = B
  my_matrix[3,] = C

  if(first_one) {
    types = c(1, 1, -1)
    densitypes = c(1, -1, -1)
  } else {
    # Second one
    types = c(-1, -1, -1)
    densitypes = c(-1, 1, -1)
  }

  alpha = 0.1
  N = 100
  Evolution = get_evol(N, my_matrix, g, densitypes, types, alpha)
  distEvolution = dist_evol(Evolution)
  plot_dist_evol(distEvolution)
  plot_evolution(Evolution, 1, N)

  distEvolution[,,dim(distEvolution)[3]]
}

test7 = function() {
  set.seed(1)
  my_matrix = tetrahedron_on_S2()
  n_elem = nrow(my_matrix)
  alpha = 0.1
  N = 1000

  ## Only one selected
  #types = rep(+1, n_elem)
  types = rspin(n_elem)
  #densitypes = rep(+1, n_elem)
  densitypes = rspin(n_elem)

  Evolution = get_evol(N, my_matrix, g, densitypes, types, alpha)
  distEvolution = dist_evol(Evolution)
  plot_dist_evol(distEvolution)
  plot_evolution(Evolution, step_min = N - 20, step_max = N)
}

test8 = function(dimshape = "sphere") {
  if(dimshape == "sphere") {
    my_matrix = tetrahedron_on_S2()
  } else if(dimshape == "circle") {
    n_elem = 4
    my_matrix = unif_on_S1(n_elem)
  }
  n_elem = nrow(my_matrix)
  N = 1000

  ## All combination of types and densitypes selected
  vectypes = combin(n_elem)
  for(i in 1:nrow(vectypes)) {
    if(i < 10) {
      evol_and_plot(my_matrix, i, vectypes, N)
    }
  }
}

########
# Misc #
########

plot_evolution = function(Evolution, step_min = 1, step_max = NA, step_by = 1, ...) {
  if(dim(Evolution)[2] == 2) {
    plot_evolution_circle(Evolution, step_min, step_max, ...)
  } else if(dim(Evolution)[2] == 3) {
    plot_evolution_sphere(Evolution, step_min, step_max, step_by)
  } else {
    stop(paste0("Evolution should live on S1 or S2, but found: S", dim(Evolution)[2]-1))
  }
}


plot_mymatrix = function(my_matrix, ...) {
  N = 1
  Evolution = array(NA, dim = c(dim(my_matrix), N))
  Evolution[,,1] = my_matrix
  plot_evolution(Evolution, step_min = 1, step_max = NA, ...)
}

###################
# Plotting on S^2 #
###################
plot_evolution_sphere = function(Evolution, step_min, step_max, step_by) {
  if(is.na(step_max)) {
    step_max = dim(Evolution)[3]
  }

  plot_sphere()
  for(step in seq(from = step_min, to = step_max, by = step_by)){
    step = floor(step)
    colpalette = c("red", "blue", "green", "yellow", "magenta", "cyan", "orange", "darkgray")
    for(k in 1:nrow(Evolution)) {
      if(k <= length(colpalette)) {
        plot_point_on_sphere(Evolution[k,,step], colpalette[k])
      } else {
        plot_point_on_sphere(Evolution[k,,step], "black")
      }
    }
  }
}

# if(debug) {
#   my_matrix = sample_on_S(n_elem = 2, dim_S = 2, seed = 1234)
#   A = my_matrix[1,]
#   B = my_matrix[2,]
#   t_max = .distance_S_great_circle(A, B) / 2  # t_max = 2*pi
#   theta = seq(from = 0, to = t_max, length.out = 100)
#   line_from_A_to_B = t(sapply(theta, function(t) {.rotated(A, B, t)}))
#   plot_sphere()
#   plot_path_on_sphere(line_from_A_to_B)
#   plot_point_on_sphere(A, "red")
#   plot_point_on_sphere(B, "blue")
#   rm(my_matrix, A, B, t_max, theta, line_from_A_to_B)
# }

###################
# Plotting on S^1 #
###################
plot_evolution_circle = function(Evolution, step_min, step_max, ...) {
  if(is.na(step_max)) {
    step_max = dim(Evolution)[3]
  }

  par(mfrow = c(1,1))
  plot_circle(...)
  for(step in seq(from = step_min, to = step_max)){
    step = floor(step)
    for(k in 1:nrow(Evolution)) {
      plot_point_on_circle(Evolution[k,,step], col = k+1, 2)
    }
  }
}

plot_circle = function(...) {
  # https://stackoverflow.com/questions/22265704/drawing-circle-in-r/22266006
  # initialize a plot
  plot(c(-1, 1), c(-1, 1), type = "n", asp = 1, ...)

  # prepare "circle data"
  radius <- 1
  theta <- seq(0, 2 * pi, length = 200)

  # draw the circle
  lines(x = radius * cos(theta), y = radius * sin(theta))
}

plot_point_on_circle = function(A, col = "red", radius = 0.1) {
  points(A[1], A[2], col = col, lwd = radius)
}

plot_path_on_circle = function(traj, col = "black", radius = 2) {
  x <- traj[,1]
  y <- traj[,2]
  lines(x, y, col = col, lwd = radius)
}

plot_segment_R_2 = function(A, B, col = "black") {
  segment_R_2 = segment_R_n_func(A, B)
  plot_path_on_circle(segment_R_2, col = col)
}
