
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
