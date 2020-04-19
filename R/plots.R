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
      rep(1, N) # solid line from head to tail
    }
  } else if(suppressWarnings(!is.na(as.numeric(trail)))) { # numeric or numeric-string
    trail = as.numeric(trail)
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
  } else if(kind_of_palette == "default") {
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    cols = gg_color_hue(nb_part)
  } else if(kind_of_palette == "blind") {
    cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    # cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
    #                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    cols = cbPalette[-1]
    if(nb_part > length(cols)) {
      cols = c(cols, rep("#999999", nb_part - length(cols)))
    } else {
      cols = cols[1:nb_part]
    }
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
                             plot_config = plot_config_func()) {
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
  df_plot_list = .adding_text_legend(df_plot_list,
                                     t_labels_nbMax,
                                     t_labels_prettyNumDigits,
                                     t_labels_tEqualText,
                                     t_labels_removeEndPoints)

  # Combine list elements
  df_plot = dplyr::bind_rows(df_plot_list, .id = "id") %>%
    dplyr::rename(points = id) %>%
    dplyr::mutate(points = factor(points))

  df_heads_tails = df_plot %>% dplyr::select(points, t, pos_x, pos_y)
  df_heads_tails = na.omit(df_heads_tails)

  df_tails = df_heads_tails %>%
    dplyr::group_by(points) %>%
    dplyr::arrange(t) %>%
    dplyr::summarise(t = t[1], pos_x = pos_x[1], pos_y = pos_y[1])

  df_heads = df_heads_tails %>%
    dplyr::group_by(points) %>%
    dplyr::arrange(-t) %>%
    dplyr::summarise(t = t[1], pos_x = pos_x[1], pos_y = pos_y[1])

  # Adding alpha
  alpha_func = get_alpha_func(trail)
  Nsizes = sapply(df_plot_list, nrow)
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

.adding_text_legend = function(df_plot_list,
                               t_labels_nbMax,
                               t_labels_prettyNumDigits,
                               t_labels_tEqualText,
                               t_labels_removeEndPoints) {
  Nsizes = sapply(df_plot_list, nrow)
  t_legend = sapply(Nsizes, function(x) {floor(seq(from = 1, to = x, length.out = t_labels_nbMax))})
  if(class(t_legend) == "numeric") { # for dealing with t_labels_nbMax = 1
    t_legend = t(as.matrix(t_legend))
  }
  for(i in 1:length(df_plot_list)) {
    df_plot_list[[i]]$t_labels = NA
    t_labels_out = prettyNum(df_plot_list[[i]]$t[t_legend[,1]], digits = t_labels_prettyNumDigits)
    if(t_labels_tEqualText) {
      t_labels_out = paste0("t=", t_labels_out)
    }
    df_plot_list[[i]]$t_labels[t_legend[,1]] = t_labels_out
  }
  if(t_labels_removeEndPoints) {
    for(i in 1:length(df_plot_list)) {
      df_plot_list[[i]]$t_labels[1] = NA
      df_plot_list[[i]]$t_labels[length(df_plot_list[[i]]$t_labels)] = NA
    }
  }
  return(df_plot_list)
}

## Plot of scent from a summary
plot_scent = function(x = "velocity", y = "acceleration", summary,
                      plot_config = plot_config_func()) {
  t = seq(from = 0, to = summary$Tmax, length.out = summary$N)
  nb_part = dim(summary$Evolution)[1]
  inputs = list(x = x, y = y)
  lab_name = list(x = "xlab", y = "ylab")
  pos = list()

  ## Change labels xlab/ylab
  for(val in c("x", "y")) {
    if(inputs[[val]] == "velocity") {
      plot_config[[lab_name[[val]]]] = "Velocity"
    } else if(inputs[[val]] == "acceleration") {
      plot_config[[lab_name[[val]]]] = "Acceleration"
    } else if(inputs[[val]] == "time") {
      plot_config[[lab_name[[val]]]] = "Time"
    } else if(grepl("dim", inputs[[val]])) {
      val_number = as.numeric(gsub("dim", "", inputs[[val]], fixed = TRUE))
      plot_config[[lab_name[[val]]]] = paste0("Dimension ", val_number)
    } else if(grepl("dist", inputs[[val]])) {
      val_number = as.numeric(gsub("dist", "", inputs[[val]], fixed = TRUE))
      plot_config[[lab_name[[val]]]] = paste0("Distance to particle ", val_number[1])
    }
  }

  ## pos_x / pos_y
  for(val in c("x", "y")) {
    if(inputs[[val]] == "velocity" || inputs[[val]] == "acceleration") {
      pos[[val]] = summary[[inputs[[val]]]]
    } else if(inputs[[val]] == "time") {
      pos[[val]] = data.frame(matrix(rep(t, nb_part), nrow = summary$N, ncol = nb_part))
    } else if(grepl("dim", inputs[[val]])) {
      val_number = as.numeric(gsub("dim", "", inputs[[val]], fixed = TRUE))
      pos[[val]] = data.frame(t(summary$Evolution[, val_number,]))
    } else if(grepl("dist", inputs[[val]])) {
      val_number = as.numeric(gsub("dist", "", inputs[[val]], fixed = TRUE))
      pos[[val]] = data.frame(t(summary$diffposition[, val_number,]))
    }
  }

  ## Plot
  df_plot_list = list()
  for(i in 1:nb_part) {
    df_plot_list[[i]] = data.frame(t = t, pos_x = pos[["x"]][,i], pos_y = pos[["y"]][,i])
  }

  generic_path_plot(df_plot_list, plot_config)
}

## Adding a circle
geom_circle = function(center = c(0,0), radius = 1,
                       linetype = "dotted",
                       color = "black",
                       alpha = 0.3,
                       npoints = 100) {
  # https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  tt <- seq(0,2 * pi,length.out = npoints)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  my_circle = data.frame(x = xx, y = yy)
  p_add = ggplot2::geom_path(mapping = ggplot2::aes(x, y),
                             linetype = linetype, color = color, alpha = alpha,
                             data = my_circle, inherit.aes = FALSE)
  return(p_add)
}
