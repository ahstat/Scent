test_that(".convert_to_list is correct in various conditions", {
  ## When input is NULL
  expect_error(.convert_to_list(NULL,
                                "numeric",
                                allow_null = FALSE,
                                normally_size_one = TRUE))
  expect_equal(.convert_to_list(NULL,
                                "numeric",
                                allow_null = TRUE,
                                normally_size_one = TRUE),
               list(NULL))

  ## When input is of size 1 normally (like manifold, number of particles)
  expect_equal(.convert_to_list(1,
                                "numeric",
                                allow_null = FALSE,
                                normally_size_one = TRUE),
               list(c(1)))
  expect_equal(.convert_to_list(c(1, 2, 3),
                                "numeric",
                                allow_null = FALSE,
                                normally_size_one = TRUE),
               as.list(c(1, 2, 3))) # list of size 3
  expect_equal(.convert_to_list(as.list(c(1, 2, 3)),
                                "numeric",
                                allow_null = FALSE,
                                normally_size_one = TRUE),
               as.list(c(1, 2, 3))) # list of size 3
  expect_equal(.convert_to_list(c("a", "b"),
                                "character",
                                allow_null = FALSE,
                                normally_size_one = TRUE),
               as.list(c("a", "b")))

  # Wrong type:
  expect_error(.convert_to_list(c("a", "b"),
                                "numeric",
                                allow_null = FALSE,
                                normally_size_one = TRUE))

  ## When input is numeric of size > 1 normally (like vector of types or densitypes)
  expect_equal(.convert_to_list(c(1, -1, 1, 1),
                                "numeric",
                                allow_null = FALSE,
                                normally_size_one = FALSE),
               list(c(1, -1, 1, 1))) # list of size 1
  expect_equal(.convert_to_list(list(c(1, -1, 1, 1), c(1, 1, 1, 1)),
                                "numeric",
                                allow_null = FALSE,
                                normally_size_one = FALSE),
               list(c(1, -1, 1, 1), c(1, 1, 1, 1))) # list of size 2

  ## When input is function or matrix
  my_matrix = matrix(1:4, nrow = 2)
  expect_equal(.convert_to_list(my_matrix,
                                "matrix",
                                allow_null = FALSE,
                                normally_size_one = FALSE),
               list(my_matrix)) # list of size 1
  expect_equal(.convert_to_list(list(my_matrix, my_matrix+1),
                                "matrix",
                                allow_null = FALSE,
                                normally_size_one = FALSE),
               list(my_matrix, my_matrix+1)) # list of size 2

  g = g_sin
  expect_equal(.convert_to_list(g,
                                "function",
                                allow_null = FALSE,
                                normally_size_one = FALSE),
               list(g)) # list of size 1
  expect_equal(.convert_to_list(list(g, g),
                                "function",
                                allow_null = FALSE,
                                normally_size_one = FALSE),
               list(g, g)) # list of size 2
})

test_that(".check_valid_NalphaTmax checks and outputs correctly the NULL name", {
  ## Error when the 3 elements are not NULL (even if correct)
  expect_error(.check_valid_NalphaTmax(N = 1, alpha = 2, Tmax = 3))
  expect_error(.check_valid_NalphaTmax(N = 1, alpha = 2, Tmax = get_Tmax(alpha = 2, N = 1)))

  ## Error when more than 1 element is NULL
  expect_error(.check_valid_NalphaTmax(N = NULL, alpha = NULL, Tmax = 3))
  expect_error(.check_valid_NalphaTmax(N = 1, alpha = NULL, Tmax = NULL))
  expect_error(.check_valid_NalphaTmax(N = NULL, alpha = 2, Tmax = NULL))
  expect_error(.check_valid_NalphaTmax(N = NULL, alpha = NULL, Tmax = NULL))

  ## Check for N, alpha, Tmax missing
  alpha = 0.006
  N = 3211
  Tmax = get_Tmax(alpha, N)
  expect_equal(.check_valid_NalphaTmax(N, alpha, NULL), "Tmax")
  expect_equal(.check_valid_NalphaTmax(N, NULL, Tmax), "alpha")
  expect_equal(.check_valid_NalphaTmax(NULL, alpha, Tmax), "N")
})

test_that("order_grid_default_func outputs the correct string up to a permutation", {
  expect_equal(sort(order_grid_default_func()),
               sort(c("manifold", "my_matrix", "g", "densitypes", "types", "N", "alpha", "Tmax")))
})

test_that(".check_valid_order_grid correctly outputs error or not", {
  ## Error if elements outside the known elements
  expect_error(.check_valid_order_grid(c("A", order_grid_default_func())))
  expect_error(.check_valid_order_grid(c(order_grid_default_func(), "A")))

  ## Error if at least one element is repeated
  expect_error(.check_valid_order_grid(c(order_grid_default_func(), order_grid_default_func())))
  expect_error(.check_valid_order_grid(c("N", "N")))

  ## Error if empty input
  expect_error(.check_valid_order_grid(c())) # please take default instead of empty

  ## Give a permutation of order_grid_default_func() in normal cases
  expect_equal(sort(.check_valid_order_grid(order_grid_default_func())),
               sort(order_grid_default_func()))
  expect_equal(sort(.check_valid_order_grid(order_grid_default_func()[-1])),
               sort(order_grid_default_func()))
  expect_equal(sort(.check_valid_order_grid(order_grid_default_func()[-c(1,2)])),
               sort(order_grid_default_func()))
  expect_equal(sort(.check_valid_order_grid(order_grid_default_func()[-c(1:3)])),
               sort(order_grid_default_func()))
  expect_equal(sort(.check_valid_order_grid(order_grid_default_func()[-c(1:4)])),
               sort(order_grid_default_func()))
  expect_equal(sort(.check_valid_order_grid(order_grid_default_func()[-c(1:6)])),
               sort(order_grid_default_func()))
  expect_equal(sort(.check_valid_order_grid(order_grid_default_func()[-c(1:7)])),
               sort(order_grid_default_func()))

  ## Give the order with first elements as expected
  expect_equal(.check_valid_order_grid("N")[1],
               "N")
  expect_equal(.check_valid_order_grid("Tmax")[1],
               "Tmax")
  expect_equal(.check_valid_order_grid(c("Tmax", "N"))[1:2],
               c("Tmax", "N"))
  expect_equal(.check_valid_order_grid(c("N", "Tmax"))[1:2],
               c("N", "Tmax"))
})

test_that("define_experiments outputs a correct data.frame", {
  ## Some elements
  my_matrix = unif_on_S1(5)
  g = g_sin
  densitypes = c(1, 1, 1, 1, 1)
  densitypes_alt = list(c(1, 1, 1, 1, 1), c(1, -1, 1, 1, 1))
  types = c(-1, -1, 1, 1, 1)
  types_alt = combin(5, 2)
  manifold = "E"
  N = 10
  N_alt = c(10, 100, 1000)
  Tmax = 3
  Tmax_alt = NULL
  alpha = NULL
  alpha_alt = 0.01
  order_grid = order_grid_default_func()
  order_grid_alt = c("N", "densitypes")

  ## 1 experiment
  exp1 = define_experiments(my_matrix, g, densitypes, types, manifold,
                            N, alpha, Tmax,
                            order_grid)
  expect_equal(nrow(exp1), 1)
  # All elements are saved as list, even numeric of size 1 like N
  expect_equal(exp1$manifold[[1]], manifold)
  expect_equal(exp1$my_matrix[[1]], my_matrix)
  expect_equal(exp1$g[[1]], g)
  expect_equal(exp1$densitypes[[1]], densitypes)
  expect_equal(exp1$types[[1]], types)
  expect_equal(exp1$N[[1]], N)
  expect_equal(exp1$alpha[[1]], alpha)
  expect_equal(exp1$Tmax[[1]], Tmax)

  ## 3 experiments
  exp3 = define_experiments(my_matrix, g, densitypes, types, manifold,
                            N_alt, alpha, Tmax,
                            order_grid)
  expect_equal(nrow(exp3), 3)

  ## 2*3 experiments
  exp6 = define_experiments(my_matrix, g, densitypes_alt, types, manifold,
                            N_alt, alpha, Tmax,
                            order_grid)
  expect_equal(nrow(exp6), 6)

  ## 2*32*3 experiments
  exp192 = define_experiments(my_matrix, g, densitypes_alt, types_alt, manifold,
                              N_alt, alpha, Tmax,
                              order_grid)
  expect_equal(nrow(exp192), 192)

  ## 3*2*32* experiments
  exp192_alt = define_experiments(my_matrix, g, densitypes_alt, types_alt, manifold,
                                  N_alt, alpha, Tmax,
                                  order_grid_alt)
  expect_equal(nrow(exp192_alt), 192)

  # N changes after for exp192 cf order_grid
  expect_equal(unlist(exp192$N), c(rep(10, 192/3), rep(100, 192/3), rep(1000, 192/3)))
  # N changes before for exp192_alt cf order_grid_alt
  expect_equal(unlist(exp192_alt$N), c(rep(c(10, 100, 1000), 192/3)))

  ## Other than alpha is NULL:
  # error because no NULL elements in the triplet (N, alpha, Tmax)
  expect_error(define_experiments(my_matrix, g, densitypes, types, manifold,
                                  N, alpha_alt, Tmax,
                                  order_grid))
  expect_error(define_experiments(my_matrix, g, densitypes, types, manifold,
                                  N, alpha_alt, Tmax_alt,
                                  order_grid), NA)
})

test_that("compute_summary_list correctly outputs the list of summary functions over the experiments", {
  my_matrix = unif_on_S1(5)
  g = g_sin
  densitypes = c(1, 1, 1, 1, 1)
  densitypes_alt = list(c(1, 1, 1, 1, 1), c(1, -1, 1, 1, 1))
  types = c(-1, -1, 1, 1, 1)
  manifold = "E"
  N = 10
  N_alt = c(10, 100, 1000)
  Tmax = 3
  alpha = NULL
  order_grid = order_grid_default_func()

  verbose = FALSE

  ## Experiments and summary
  exp1 = define_experiments(my_matrix, g, densitypes, types, manifold,
                            N, alpha, Tmax,
                            order_grid)
  summary1 = compute_summary_list(exp1, verbose)
  exp3 = define_experiments(my_matrix, g, densitypes, types, manifold,
                            N_alt, alpha, Tmax,
                            order_grid)
  summary3 = compute_summary_list(exp3, verbose)
  exp6 = define_experiments(my_matrix, g, densitypes_alt, types, manifold,
                            N_alt, alpha, Tmax,
                            order_grid)
  summary6 = compute_summary_list(exp6, verbose)

  ## Tests

  # Sizes
  expect_equal(length(summary1), 1)
  expect_equal(length(summary3), 3)
  expect_equal(length(summary6), 6)

  # Correspondance each summary follow the experiments
  expect_equal(sapply(summary6, function(x){x$N}), unlist(exp6$N))
  expect_equal(sapply(summary6, function(x){x$N}), unlist(exp6$N))
  expect_equal(lapply(summary6, function(x){x$Evolution[,,1]}), exp6$my_matrix)
})

test_that("filter_summary_list correctly filters velocity", {
  my_matrix = unif_on_S1(5)
  g = g_sin
  densitypes_alt = list(c(1, 1, 1, 1, 1), c(1, -1, 1, 1, 1))
  types = c(-1, -1, 1, 1, 1)
  manifold = "E"
  N_alt = c(10, 100, 1000)
  Tmax = 3
  alpha = NULL
  order_grid = order_grid_default_func()

  verbose = FALSE

  ## Experiments and summary
  exp6 = define_experiments(my_matrix, g, densitypes_alt, types, manifold,
                            N_alt, alpha, Tmax,
                            order_grid)
  summary6 = compute_summary_list(exp6, verbose)

  ## Tests

  ##
  # At least one particle has speed > a certain constant
  ##
  one_0_1 = filter_summary_list(summary6,
                                "velocity", "at the end is",
                                greater_than = 0.1, "for at least one particle")
  one_0_25 = filter_summary_list(summary6,
                                 "velocity", "at the end is",
                                 greater_than = 0.25, "for at least one particle")
  one_0_35 = filter_summary_list(summary6,
                                 "velocity", "at the end is",
                                 greater_than = 0.35, "for at least one particle")

  # Check that for one_0_35, the last element is > 0.35 for one summary for at least one particle
  expect_equal(any(one_0_35[[1]]$velocity[10,] > 0.35), TRUE)

  # Less and less remaining elements when the threshold increases
  expect_equal(length(one_0_1) > length(one_0_25), TRUE)
  expect_equal(length(one_0_25) > length(one_0_35), TRUE)

  # Warning when no element remaining
  expect_warning(filter_summary_list(summary6,
                                     "velocity", "at the end is",
                                     greater_than = 0.5, "for at least one particle"))
  expect_equal(suppressWarnings({filter_summary_list(summary6,
                                                     "velocity", "at the end is",
                                                     greater_than = 0.5, "for at least one particle")}),
               list())

  ##
  # All particles have speed > a certain constant
  ##
  all_0_1 = filter_summary_list(summary6,
                                "velocity", "at the end is",
                                greater_than = 0.1, "for all particles")
  all_0_15 = filter_summary_list(summary6,
                                 "velocity", "at the end is",
                                 greater_than = 0.15, "for all particles")
  all_0_2 = filter_summary_list(summary6,
                                "velocity", "at the end is",
                                greater_than = 0.2, "for all particles")

  # Check that for all_0_2, the last element is > 0.2 for one summary
  expect_equal(all(all_0_2[[1]]$velocity[10,] > 0.2), TRUE)

  # Less and less remaining elements when the threshold increases
  expect_equal(length(all_0_1) > length(all_0_15), TRUE)
  expect_equal(length(all_0_15) > length(all_0_2), TRUE)

  # Warning when no element remaining
  expect_warning(filter_summary_list(summary6,
                                     "velocity", "at the end is",
                                     greater_than = 0.3, "for all particles"))
  expect_equal(suppressWarnings({filter_summary_list(summary6,
                                                     "velocity", "at the end is",
                                                     greater_than = 0.3, "for all particles")}),
               list())
})

test_that("multiplot_scent gives the correct number of plots", {
  my_matrix = unif_on_S1(5)
  g = g_sin
  densitypes_alt = list(c(1, 1, 1, 1, 1), c(1, -1, 1, 1, 1))
  types = c(-1, -1, 1, 1, 1)
  manifold = "E"
  N_alt = c(10, 100, 1000)
  Tmax = 3
  alpha = NULL
  order_grid = order_grid_default_func()

  verbose = FALSE

  ## Experiments and summary
  my_experiments = define_experiments(my_matrix, g, densitypes_alt, types, manifold,
                            N_alt, alpha, Tmax,
                            order_grid)
  summary_list = compute_summary_list(my_experiments, verbose)

  my_pos_xy = data.frame(pos_x = "time", pos_y = "velocity", stringsAsFactors = FALSE)
  my_pos_xy_alt = data.frame(pos_x = c("time", "time", "velocity"),
                             pos_y = c("velocity", "acceleration", "acceleration"),
                             stringsAsFactors = FALSE)

  plot_1 = multiplot_scent(my_experiments, my_pos_xy, summary_list, config_for_plot_func())
  plot_alt = multiplot_scent(my_experiments, my_pos_xy_alt, summary_list, config_for_plot_func())

  ## Add a custom title
  my_experiments$title = paste0("densitypes = (", sapply(my_experiments$densitypes,
                                                         function(x) {paste(x, collapse = ", ")}), ")",
                                " / N = ", my_experiments$N)

  plot_1_title = multiplot_scent(my_experiments, my_pos_xy, summary_list, config_for_plot_func())
  plot_alt_title = multiplot_scent(my_experiments, my_pos_xy_alt, summary_list, config_for_plot_func())

  ## Test that number of plots is correct
  expect_equal(length(plot_1), 6) # 6 experiments and 1 choice of pos_xy
  expect_equal(length(plot_alt), 6*3) # 6 experiments and 3 choices of pos_xy
  expect_equal(length(plot_1_title), 6)
  expect_equal(length(plot_alt_title), 6*3)

  ## Test that error when factors instead of characters for my_pos_xy
  my_pos_xy_wrong = data.frame(pos_x = "time", pos_y = "velocity")
  expect_error(multiplot_scent(my_experiments, my_pos_xy_wrong, summary_list, config_for_plot_func()))
})

test_that("older tests still give some results on the circle", {
  # Case 1
  n_elem = 4
  my_matrix = unif_on_S1(n_elem)
  g = g_sin
  densitypes = combin(n_elem, 2)
  types = rep(1, n_elem)
  manifold = "S"
  N = 10
  alpha = 1
  Tmax = NULL

  ## Experiments and summary
  my_experiments = define_experiments(my_matrix, g, densitypes, types, manifold,
                                      N, alpha, Tmax)
  summary_list = compute_summary_list(my_experiments, verbose = FALSE)

  ## Plotting
  my_pos_xy = data.frame(pos_x = "time", pos_y = "velocity",
                         stringsAsFactors = FALSE)
  p_list = multiplot_scent(my_experiments, my_pos_xy, summary_list, config_for_plot_func())
  # ggsave_func(p_list, outfile = "multipage_circle.pdf")

  expect_error(p_list, NA)
})

test_that("older tests still give some results on the tetrahedron", {
  # Case 2
  my_matrix = tetrahedron_on_S2()
  n_elem = nrow(my_matrix)
  g = g_sin
  densitypes = combin(n_elem, 2)
  types = rep(1, n_elem)
  manifold = "S"
  N = 10
  alpha = 1
  Tmax = NULL

  ## Experiments and summary
  my_experiments = define_experiments(my_matrix, g, densitypes, types, manifold,
                                      N, alpha, Tmax)
  summary_list = compute_summary_list(my_experiments, verbose = FALSE)

  ## Plotting
  my_pos_xy = data.frame(pos_x = "time", pos_y = "velocity",
                         stringsAsFactors = FALSE)
  p_list = multiplot_scent(my_experiments, my_pos_xy, summary_list, config_for_plot_func())
  # ggsave_func(p_list, outfile = "multipage_sphere.pdf")

  expect_error(p_list, NA)
})
