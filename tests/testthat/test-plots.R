test_that("get_alpha_func returns vector of increasing alphas of correct size", {
  for(trail in c("fade", "solid", 1, 2, 3, 5, 10, 20, 100)) {
    alpha_func = get_alpha_func(trail)
    for(N in c(1, 2, 3, 5, 10, 20, 100)) {
      expect_equal(length(alpha_func(N)), N) # alphas have same size as N
      expect_equal(all(diff(alpha_func(N)) >= 0), TRUE) # alphas increase from tail to head
      expect_equal(all(alpha_func(N) >= 0), TRUE) # alphas >= 0
      expect_equal(all(alpha_func(N) <= 1), TRUE) # alphas <= 1
    }
  }
})

test_that("generic_palette returns a palette of colors of correct size", {
  for(kind_of_palette in c("first_red", "blind", "default")) {
    for(nb_part in c(1, 2, 3, 5, 10, 20, 100)) {
      expect_equal(length(generic_palette(nb_part, kind_of_palette)), nb_part)
    }
  }
})

test_that("plot_config_func returns a list with the correct elements", {
  plot_config = plot_config_func()
  expect_equal(length(plot_config), 17)
  expect_true("plotting_option" %in% names(plot_config))
  expect_true("trail" %in% names(plot_config))
  expect_true("kind_of_palette" %in% names(plot_config))
  expect_true("xlab" %in% names(plot_config))
  expect_true("ylab" %in% names(plot_config))
  expect_true("main_title" %in% names(plot_config))
  expect_true("legend_title" %in% names(plot_config))
  expect_true("heads_alpha" %in% names(plot_config))
  expect_true("heads_shape" %in% names(plot_config))
  expect_true("tails_alpha" %in% names(plot_config))
  expect_true("tails_shape" %in% names(plot_config))
  expect_true("t_labels" %in% names(plot_config))
  expect_true("t_labels_nbMax" %in% names(plot_config))
  expect_true("t_labels_prettyNumDigits" %in% names(plot_config))
  expect_true("t_labels_size" %in% names(plot_config))
  expect_true("t_labels_tEqualText" %in% names(plot_config))
  expect_true("t_labels_removeEndPoints" %in% names(plot_config))
})

test_that(".adding_text_legend correctly adds the time legend column", {
  Tmax = 1
  N = 20
  df_plot1 = data.frame(t = seq(from = 0, to = Tmax, length.out = N),
                        pos_x = 1:N,
                        pos_y = 1:N)
  df_plot2 = data.frame(t = seq(from = 0, to = Tmax, length.out = N),
                        pos_x = 1:N,
                        pos_y = rev(1:N))
  df_plot_list = list()
  df_plot_list[["partic1"]] = df_plot1
  df_plot_list[["partic2"]] = df_plot2

  for(t_labels_nbMax in c(1, 3, 10, 30)) {
    for(t_labels_prettyNumDigits in c(0, 2, 10)) {
      for(t_labels_tEqualText in c(FALSE, TRUE)) {
        for(t_labels_removeEndPoints in c(FALSE, TRUE)) {
          df_plot_list_out = .adding_text_legend(df_plot_list,
                                                 t_labels_nbMax,
                                                 t_labels_prettyNumDigits,
                                                 t_labels_tEqualText,
                                                 t_labels_removeEndPoints)
          # give same list sizes as in input
          expect_equal(length(df_plot_list_out), 2)
          df1 = df_plot_list_out[[1]]
          df2 = df_plot_list_out[[2]]
          # labels t_labels have been added
          expect_true("t_labels" %in% colnames(df2))
          if(!t_labels_tEqualText) {
            df1_test = df1[which(!is.na(df1$t_labels)),]
            # t_labels is approximatively t
            if(t_labels_prettyNumDigits > 0) {
              expect_equal(df1_test$t, as.numeric(df1_test$t_labels), tolerance = 1e-1)
            } else {
              expect_equal(df1_test$t, as.numeric(df1_test$t_labels), tolerance = 1e0)
            }
          }
          labels1 = df1$t_labels
          expect_true(sum(!is.na(labels1)) <= t_labels_nbMax)
          if(t_labels_removeEndPoints) {
            # tail is missing when remove end points
            expect_true(is.na(labels1[1]))
            # head is missing when remove end points
            expect_true(is.na(labels1[length(labels1)]))
          }
        }
      }
    }
  }
})

test_that("generic_path_plot does not throw error", {
  Tmax = 1
  N = 20
  df_plot1 = data.frame(t = seq(from = 0, to = Tmax, length.out = N),
                        pos_x = 1:N,
                        pos_y = 1:N)
  df_plot2 = data.frame(t = seq(from = 0, to = Tmax, length.out = N),
                        pos_x = 1:N,
                        pos_y = rev(1:N))
  df_plot_list = list()
  df_plot_list[["partic1"]] = df_plot1
  df_plot_list[["partic2"]] = df_plot2

  plot_config = plot_config_func()

  plot_config$plotting_option = 1
  plot_config$kind_of_palette = "first_red"
  expect_error(generic_path_plot(df_plot_list, plot_config), NA)

  plot_config$plotting_option = 2
  plot_config$kind_of_palette = "default"
  expect_error(generic_path_plot(df_plot_list, plot_config), NA)

  plot_config$plotting_option = 3
  plot_config$kind_of_palette = "blind"
  expect_error(generic_path_plot(df_plot_list, plot_config), NA)
})

test_that("plot_scent does not throw error", {
  my_matrix = unif_on_S1(5)
  g = g_sin
  densitypes = c(1, 1, 1, 1, 1)
  types = c(-1, -1, 1, 1, 1)
  manifold = "E"

  N = 1000
  alpha = 0.003
  Evolution = get_evol(N, my_matrix, g, densitypes, types, alpha, manifold)
  summary = summary_func(Evolution, manifold, alpha)

  p1 = plot_scent(x = "velocity", y = "acceleration", summary,
                  plot_config_func(plotting_option = 1,
                                   t_labels = FALSE,
                                   kind_of_palette = "default"))
  expect_error(p1, NA)

  p2 = plot_scent(x = "time", y = "velocity", summary,
                  plot_config_func(plotting_option = 1, t_labels = TRUE, kind_of_palette = "default"))
  expect_error(p2, NA) # > 0.5 each

  p3 = plot_scent(x = "time", y = "acceleration", summary,
                  plot_config_func(plotting_option = 1, t_labels = TRUE, kind_of_palette = "default"))
  expect_error(p3, NA)

  p4 = plot_scent(x = "dim1", y = "dim2", summary,
                  plot_config_func(plotting_option = 1, t_labels = TRUE))
  expect_error(p4, NA)

  p4_with_circle = p4 + geom_circle() + ggplot2::coord_equal()
  expect_error(p4_with_circle, NA)

  p5 = plot_scent(x = "dist1", y = "dist2", summary,
                  plot_config_func(plotting_option = 1, t_labels = TRUE))
  expect_error(p5, NA)
})

test_that("plot_scent does not throw error (some additional plots)", {
  ## Test 6
  p6 = list()
  p6[[1]] = list()
  p6[[2]] = list()

  n_elem = 3
  my_matrix = unif_on_S1(n_elem)
  manifold = "E"
  alpha = 0.1
  N = 100

  for(test_id in c(1, 2)) {
    if(test_id == 1) {
      types = c(1, 1, -1)
      densitypes = c(1, -1, -1)
    } else {
      types = c(-1, -1, -1)
      densitypes = c(-1, 1, -1)
    }

    types_name = paste0("(", paste(types, collapse = ", "), ")")
    densitypes_name = paste0("(", paste(densitypes, collapse = ", "), ")")

    Evolution = get_evol(N, my_matrix, g, densitypes, types, alpha, manifold)
    summary = summary_func(Evolution, manifold, alpha)
    p6[[test_id]][[1]] = plot_scent("dim1", "dim2", summary, plot_config = plot_config_func()) +
      geom_circle() + ggplot2::coord_equal() +
      ggplot2::ggtitle(paste0("types ", types_name, " and densitypes ", densitypes_name))
    p6[[test_id]][[2]] = plot_scent("dist1", "dist2", summary, plot_config = plot_config_func()) +
      ggplot2::coord_equal() +
      ggplot2::ggtitle(paste0("types ", types_name, " and densitypes ", densitypes_name))
  }

  expect_error(p6[[1]][[1]], NA)
  expect_error(p6[[1]][[2]], NA)
  expect_error(p6[[2]][[1]], NA)
  expect_error(p6[[2]][[2]], NA)

  ## Test 7
  p7 = list()

  set.seed(1)
  my_matrix = tetrahedron_on_S2()
  n_elem = nrow(my_matrix)
  alpha = 0.008
  N = 1000
  types = rspin(n_elem)
  densitypes = rspin(n_elem)
  manifold = "S"

  Evolution = get_evol(N, my_matrix, g, densitypes, types, alpha, manifold)
  summary = summary_func(Evolution, manifold, alpha)
  plot_config = plot_config_func(plotting_option = 1, kind_of_palette = "default", trail = 500)
  p7[[1]] = plot_scent("dim1", "dim2", summary, plot_config = plot_config) +
    geom_circle() + ggplot2::coord_equal()
  p7[[2]] = plot_scent("dist1", "dist3", summary, plot_config = plot_config) +
    ggplot2::coord_equal()
  p7[[3]] = plot_scent("velocity", "acceleration", summary, plot_config = plot_config) +
    ggplot2::coord_equal()

  expect_error(p7[[1]], NA)
  expect_error(p7[[2]], NA)
  expect_error(p7[[3]], NA)
})
