##############################
# Configuration of the plots #
##############################

## Blank theme
theme_blank = function() {
  ggplot2::theme(axis.line = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 legend.position = "none",
                 panel.background = ggplot2::element_blank(),
                 panel.border = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 plot.background = ggplot2::element_blank())
}

## Fading shape after the head
get_alpha_func = function(trail = "fade") {
  if(trail == "fade") {
    alpha_func = function(N) {
      (1:N)/N # fading from solid (at head) to transparent (at tail)
    }
  } else if(trail == "solid") {
    alpha_func = function(N) {
      1 # solid line from head to tail
    }
  } else if(is.numeric(trail)) {
    base_trail = (1:trail)/trail
    alpha_func = function(N) {
      out = tail(base_trail, N)
      N0 = length(out)
      if(N > N0) {
        out = c(rep(0, N - N0), out)
      }
      return(out) # fading from solid (at head) to transparent (at head - trail)
    }
  } else {
    stop("trail must be 'fade', 'solid' or a positive integer")
  }
}

## Palettes of color
generic_palette = function(nb_part, kind_of_palette) {
  if(kind_of_palette == "first_red") {
    cols = c("red")
    cols = c(cols, rep("black", nb_part - 1))
  } else {
    stop("TODO, implement in generic_palette function")
  }
  return(cols)
}

## Global plotting configuration
plot_config_func = function(plotting_option = 2, # 1 with legend / 2 without legend / 3 blank theme
                            trail = "fade", # "solid", "fade" or integer
                            kind_of_palette = "first_red",
                            xlab = "Position x",
                            ylab = "Position y",
                            main_title = NULL,
                            legend_title = "Particle",
                            heads_alpha = 1, # full color
                            heads_shape = 19, # filled point
                            tails_alpha = 0.3,
                            tails_shape = 1, # circled point
                            t_labels = TRUE,
                            t_labels_nbMax = 5,
                            t_labels_prettyNumDigits = 2,
                            t_labels_size = 2.5,
                            t_labels_tEqualText = TRUE,
                            t_labels_removeEndPoints = TRUE) {
  list(plotting_option = plotting_option,
       trail = trail,
       kind_of_palette = kind_of_palette,
       xlab = xlab,
       ylab = ylab,
       main_title = main_title,
       legend_title = legend_title,
       heads_alpha = heads_alpha,
       heads_shape = heads_shape,
       tails_alpha = tails_alpha,
       tails_shape = tails_shape,
       t_labels = t_labels,
       t_labels_nbMax = t_labels_nbMax,
       t_labels_prettyNumDigits = t_labels_prettyNumDigits,
       t_labels_size = t_labels_size,
       t_labels_tEqualText = t_labels_tEqualText,
       t_labels_removeEndPoints = t_labels_removeEndPoints)
}

## Generic path plotting
generic_path_plot = function(df_plot_list,
                             plot_config = list(plotting_option = 2, # 1 with legend / 2 without legend / 3 blank theme
                                                trail = "fade", # "solid", "fade" or integer
                                                kind_of_palette = "first_red",
                                                xlab = "Position x",
                                                ylab = "Position y",
                                                main_title = NULL,
                                                legend_title = "Particle",
                                                heads_alpha = 1, # full color
                                                heads_shape = 19, # filled point
                                                tails_alpha = 0.3,
                                                tails_shape = 1, # circled point
                                                t_labels = TRUE,
                                                t_labels_nbMax = 5,
                                                t_labels_prettyNumDigits = 2,
                                                t_labels_size = 2.5,
                                                t_labels_tEqualText = TRUE,
                                                t_labels_removeEndPoints = TRUE)) {
  ## Attaching variables
  plotting_option = plot_config$plotting_option
  trail = plot_config$trail
  kind_of_palette = plot_config$kind_of_palette
  xlab = plot_config$xlab
  ylab = plot_config$ylab
  main_title = plot_config$main_title
  legend_title = plot_config$legend_title
  heads_alpha = plot_config$heads_alpha
  heads_shape = plot_config$heads_shape
  tails_alpha = plot_config$tails_alpha
  tails_shape = plot_config$tails_shape
  t_labels = plot_config$t_labels
  t_labels_nbMax = plot_config$t_labels_nbMax
  t_labels_prettyNumDigits = plot_config$t_labels_prettyNumDigits
  t_labels_size = plot_config$t_labels_size
  t_labels_tEqualText = plot_config$t_labels_tEqualText
  t_labels_removeEndPoints = plot_config$t_labels_removeEndPoints

  ## Color palette
  nb_part = length(df_plot_list)
  if(nb_part < 1) {
    stop("Need at least one point to plot")
  }
  cols = generic_palette(nb_part, kind_of_palette) # c("partic1" = "red", "partic2" = "black")

  ## Check
  if(any(sapply(df_plot_list, function(x){is.unsorted(x$t)}))) {
    stop("Each element of df_plot_list must be sorted by time")
  }

  ## Preparation
  Nsizes = sapply(df_plot_list, nrow)

  # Adding text legend
  t_legend = sapply(Nsizes, function(x) {floor(seq(from = 1, to = x, length.out = t_labels_nbMax))})
  for(i in 1:length(df_plot_list)) {
    df_plot_list[[i]]$t_labels = NA
    t_labels_out = prettyNum(df_plot_list[[i]]$t[t_legend[,1]], digits = t_labels_prettyNumDigits)
    if(t_labels_tEqualText) {
      t_labels_out = paste0("t=", t_labels_out)
    }
    if(t_labels_removeEndPoints) {
      t_labels_out[1] = NA
      t_labels_out[length(t_labels_out)] = NA
    }
    df_plot_list[[i]]$t_labels[t_legend[,1]] = t_labels_out
  }

  # Combine list elements
  df_plot = bind_rows(df_plot_list, .id = "id") %>%
    rename(points = id) %>%
    mutate(points = factor(points))

  df_tails = df_plot %>%
    group_by(points) %>%
    arrange(t) %>%
    summarise(t = t[1], pos_x = pos_x[1], pos_y = pos_y[1])

  df_heads = df_plot %>%
    group_by(points) %>%
    arrange(-t) %>%
    summarise(t = t[1], pos_x = pos_x[1], pos_y = pos_y[1])

  # Adding alpha
  alpha_func = get_alpha_func(trail)
  df_plot$alpha = as.vector(sapply(Nsizes, alpha_func))
  df_tails$alpha = tails_alpha
  df_heads$alpha = heads_alpha

  ## Plotting
  p = ggplot2::ggplot(df_plot, ggplot2::aes(x = pos_x, y = pos_y, color = points, alpha = alpha, label = t_labels)) +
    ggplot2::geom_path(na.rm = TRUE) + ggplot2::guides(alpha=FALSE) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::labs(color = legend_title, title = main_title)

  if(t_labels) {
    p = p + ggplot2::geom_label(size = t_labels_size, na.rm = TRUE, hjust = 0.5)
  }

  if(!is.null(cols)) {
    if(is.null(names(cols))) { # align/check cols with names of df_plot_list
      if(!is.null(names(df_plot_list))) {
        names(cols) = names(df_plot_list)
      } else {
        names(cols) = levels(df_plot$points)
      }
    }
    p = p + ggplot2::scale_colour_manual(values = cols)
  }

  if(!is.null(tails_shape)) {
    p = p +
      ggplot2::geom_point(data = df_tails, shape = tails_shape, na.rm = TRUE)
  }

  if(!is.null(heads_shape)) {
    p = p +
      ggplot2::geom_point(data = df_heads, shape = heads_shape, na.rm = TRUE)
  }

  if(plotting_option == 1) {
    p = p + ggplot2::theme_bw()
  } else if(plotting_option == 2) {
    p = p + ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")
  } else if(plotting_option == 3) {
    p = p + theme_blank()
  }
  return(p)
}

## Plot of scent from a summary
plot_scent = function(x = "velocity", y = "acceleration", summary,
                      plot_config = plot_config_func()) {
  t = seq(from = 0, to = summary$Tmax, length.out = summary$N)
  nb_part = dim(summary$Evolution)[1]

  ## Change labels
  if(x == "velocity") {
    plot_config$xlab = "Velocity"
  } else if(x == "acceleration") {
    plot_config$xlab = "Acceleration"
  } else if(x == "time") {
    plot_config$xlab = "Time"
  } else if(grepl("dim", x)) {
    x_number = as.numeric(gsub("dim", "", x, fixed = TRUE))
    plot_config$xlab = paste0("Dimension ", x_number)
  }

  if(y == "velocity") {
    plot_config$ylab = "Velocity"
  } else if(y == "acceleration") {
    plot_config$ylab = "Acceleration"
  } else if(y == "time") {
    plot_config$ylab = "Time"
  } else if(grepl("dim", y)) {
    y_number = as.numeric(gsub("dim", "", y, fixed = TRUE))
    plot_config$ylab = paste0("Dimension ", y_number)
  }

  ## pos_x / pos_y
  if(x == "velocity" || x == "acceleration") {
    pos_x = summary[[x]]
  } else if(x == "time") {
    pos_x = data.frame(matrix(rep(t, nb_part), nrow = summary$N, ncol = nb_part))
  } else if(grepl("dim", x)) {
    x_number = as.numeric(gsub("dim", "", x, fixed = TRUE))
    pos_x = data.frame(t(summary$Evolution[, x_number,]))
  }

  if(y == "velocity" || y == "acceleration") {
    pos_y = summary[[y]]
  } else if(y == "time") {
    pos_y = data.frame(matrix(rep(t, nb_part), nrow = summary$N, ncol = nb_part))
  } else if(grepl("dim", y)) {
    y_number = as.numeric(gsub("dim", "", y, fixed = TRUE))
    pos_y = data.frame(t(summary$Evolution[, y_number,]))
  }

  ## Plot
  df_plot_list = list()
  for(i in 1:nb_part) {
    df_plot_list[[i]] = data.frame(t = t, pos_x = pos_x[,i], pos_y = pos_y[,i])
  }

  generic_path_plot(df_plot_list, plot_config)
}

# plot_evolution = function(Evolution, step_min = 1, step_max = NA, step_by = 1, ...) {
#   if(dim(Evolution)[2] == 2) {
#     plot_evolution_circle(Evolution, step_min, step_max, ...)
#   } else if(dim(Evolution)[2] == 3) {
#     plot_evolution_sphere(Evolution, step_min, step_max, step_by)
#   } else {
#     stop(paste0("Evolution should live on S1 or S2, but found: S", dim(Evolution)[2]-1))
#   }
# }
#

# plot_mymatrix = function(my_matrix, ...) {
#   N = 1
#   Evolution = array(NA, dim = c(dim(my_matrix), N))
#   Evolution[,,1] = my_matrix
#   plot_evolution(Evolution, step_min = 1, step_max = NA, ...)
# }
#
# ###################
# # Plotting on S^2 #
# ###################
# plot_evolution_sphere = function(Evolution, step_min, step_max, step_by) {
#   if(is.na(step_max)) {
#     step_max = dim(Evolution)[3]
#   }
#
#   plot_sphere()
#   for(step in seq(from = step_min, to = step_max, by = step_by)){
#     step = floor(step)
#     colpalette = c("red", "blue", "green", "yellow", "magenta", "cyan", "orange", "darkgray")
#     for(k in 1:nrow(Evolution)) {
#       if(k <= length(colpalette)) {
#         plot_point_on_sphere(Evolution[k,,step], colpalette[k])
#       } else {
#         plot_point_on_sphere(Evolution[k,,step], "black")
#       }
#     }
#   }
# }
#
# plot_sphere = function() {
#   # https://stackoverflow.com/questions/34539268
#   rgl::spheres3d(0, 0, 0, lit = FALSE, color = "white")
#   rgl::spheres3d(0, 0, 0, radius = 1.01, lit = FALSE, color = "black", front = "lines")
# }
#
# plot_point_on_sphere = function(A, col = "red", radius = 0.1) {
#   rgl::spheres3d(A[1], A[2], A[3], col = col, radius = radius)
# }
#
# plot_path_on_sphere = function(traj, col = "black", radius = 0.02) {
#   # traj is a general trajectory
#   x <- traj[,1]
#   y <- traj[,2]
#   z <- traj[,3]
#   rgl::spheres3d(x, y, z, col = col, radius = radius)
# }
#
# # if(debug) {
# #   my_matrix = sample_on_S(n_elem = 2, dim_S = 2, seed = 1234)
# #   A = my_matrix[1,]
# #   B = my_matrix[2,]
# #   t_max = .distance_S_great_circle(A, B) / 2  # t_max = 2*pi
# #   theta = seq(from = 0, to = t_max, length.out = 100)
# #   line_from_A_to_B = t(sapply(theta, function(t) {.rotated(A, B, t)}))
# #   plot_sphere()
# #   plot_path_on_sphere(line_from_A_to_B)
# #   plot_point_on_sphere(A, "red")
# #   plot_point_on_sphere(B, "blue")
# #   rm(my_matrix, A, B, t_max, theta, line_from_A_to_B)
# # }
#
# plot_all_points_on_sphere = function(i, my_matrix, radius = 0.05,
#                                      col_i = "red",
#                                      col_others = "black") {
#   plot_point_on_sphere(my_matrix[i,], col_i, radius)
#   for(j in (1:nrow(my_matrix))[-i]) {
#     plot_point_on_sphere(my_matrix[j,], col_others, radius)
#   }
# }
#
# segment_R_n_func = function(A, B) {
#   # on the tangent space in R^{n-1}. Not ok for segment in S^{n-1}
#   theta = seq(from = 0, to = 1, length.out = 100)
#   segment_R_n = t(sapply(theta, function(t) {t * A + (1-t) * B}))
#   return(segment_R_n)
# }
#
# segment_S_n_func = function(A, B, t_max = "segment") {
#   if(t_max == "line") {
#     t_max = 2*pi
#   } else if(t_max == "segment") {
#     t_max = .distance_S_great_circle(A, B)
#   } else if(t_max == "semisegment") {
#     t_max = .distance_S_great_circle(A, B) / 2
#   }
#   theta = seq(from = 0, to = t_max, length.out = 100)
#   line_from_A_to_B = t(sapply(theta, function(t) {.rotated(A, B, t)}))
#   return(line_from_A_to_B)
# }
#
# plot_segment_R_n = function(A, B, col = "black") {
#   segment_R_n = segment_R_n_func(A, B)
#   plot_path_on_sphere(segment_R_n, col = col)
# }
#
# plot_segment_S_n = function(A, B, col = "black") {
#   segment_S_n = segment_S_n_func(A, B)
#   plot_path_on_sphere(segment_S_n, col = col)
# }
#
# ###################
# # Plotting on S^1 #
# ###################
# plot_evolution_circle = function(Evolution, step_min, step_max, ...) {
#   if(is.na(step_max)) {
#     step_max = dim(Evolution)[3]
#   }
#
#   par(mfrow = c(1,1))
#   plot_circle(...)
#   for(step in seq(from = step_min, to = step_max)){
#     step = floor(step)
#     for(k in 1:nrow(Evolution)) {
#       plot_point_on_circle(Evolution[k,,step], col = k+1, 2)
#     }
#   }
# }
#
# plot_circle = function(...) {
#   # https://stackoverflow.com/questions/22265704/drawing-circle-in-r/22266006
#   # initialize a plot
#   plot(c(-1, 1), c(-1, 1), type = "n", asp = 1, ...)
#
#   # prepare "circle data"
#   radius <- 1
#   theta <- seq(0, 2 * pi, length = 200)
#
#   # draw the circle
#   lines(x = radius * cos(theta), y = radius * sin(theta))
# }
#
# plot_point_on_circle = function(A, col = "red", radius = 0.1) {
#   points(A[1], A[2], col = col, lwd = radius)
# }
#
# plot_path_on_circle = function(traj, col = "black", radius = 2) {
#   x <- traj[,1]
#   y <- traj[,2]
#   lines(x, y, col = col, lwd = radius)
# }
#
# plot_segment_R_2 = function(A, B, col = "black") {
#   segment_R_2 = segment_R_n_func(A, B)
#   plot_path_on_circle(segment_R_2, col = col)
# }
