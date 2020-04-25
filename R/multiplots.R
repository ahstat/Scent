# Convert any element to a list element (for automatization)
.convert_to_list = function(x, normal_class, allow_null = FALSE, normally_size_one = FALSE) {
  if(is.null(x)) {
    if(allow_null) {
      return(list(NULL))
    } else {
      stop("input is NULL and it should not be for this object")
    }
  } else if("list" %in% normal_class) {
    stop("don't know how to deal with list as normal_class")
  } else {
    if(class(x) %in% normal_class) {
      if(normally_size_one == FALSE) {
        x_out = list(x) # convert c(1, 1, -1, 1, 1) to a list of size 1: [c(1, 1, -1, 1, 1)]
      } else {
        x_out = as.list(x) # convert c(10, 100, 1000) to a list of size 3: [10, 100, 1000]
      }
    } else if(class(x) == "list") {
      x_out = x
    } else {
      stop(paste0("Class of this object must be ", normal_class, " or list."))
    }
  }
  return(x_out)
}

# Check validity of a triplet (N, alpha, Tmax) and outputs the evaluation of the NULL element
.check_valid_NalphaTmax = function(N, alpha, Tmax) {
  if(is.null(N) + is.null(alpha) + is.null(Tmax) != 1) {
    stop("Need input of N/alpha, N/Tmax or alpha/Tmax, the remaining one should be NULL")
  }
  if(is.null(N)) {
    return("N")
  } else if(is.null(alpha)) {
    return("alpha")
  } else if(is.null(Tmax)) {
    return("Tmax")
  } else {
    stop("error in .check_valid_NalphaTmax")
  }
}

# Order of the grid conditions to loop on
order_grid_default_func = function() {
  c("manifold", "my_matrix",
    "g", "densitypes", "types",
    "N", "alpha", "Tmax")
}

# Check validity of order of the grid conditions to loop on
.check_valid_order_grid = function(order_grid) {
  order_grid_default = order_grid_default_func()
  if(max(rle(sort(order_grid))[[1]]) > 1) {
    stop("Some repeated elements in order_grid")
  }
  if(!all(order_grid %in% order_grid_default)) {
    stop("order_grid must contains elements of ", paste(order_grid_default, collapse = ", "))
  }
  return(c(order_grid, setdiff(order_grid_default, order_grid)))
}

# Define data frame of experiments to realize
define_experiments = function(my_matrix, g, densitypes, types, manifold,
                              N = NULL, alpha = NULL, Tmax = NULL,
                              order_grid = order_grid_default_func()) {
  if(is.null(order_grid)) {
    order_grid = order_grid_default_func()
  }
  whichNULL = .check_valid_NalphaTmax(N, alpha, Tmax)
  valid_order_grid = .check_valid_order_grid(order_grid)

  unordered_elem_list = list()
  unordered_elem_list[["manifold"]] = .convert_to_list(manifold, "character", normally_size_one = TRUE)
  unordered_elem_list[["my_matrix"]] = .convert_to_list(my_matrix, c("matrix"))
  unordered_elem_list[["g"]] = .convert_to_list(g, c("function"))
  unordered_elem_list[["densitypes"]] = .convert_to_list(densitypes, "numeric", normally_size_one = FALSE)
  unordered_elem_list[["types"]] = .convert_to_list(types, "numeric", normally_size_one = FALSE)
  unordered_elem_list[["N"]] = .convert_to_list(N, c("numeric", "integer"), allow_null = TRUE, normally_size_one = TRUE)
  unordered_elem_list[["alpha"]] = .convert_to_list(alpha, "numeric", allow_null = TRUE, normally_size_one = TRUE)
  unordered_elem_list[["Tmax"]] = .convert_to_list(Tmax, "numeric", allow_null = TRUE, normally_size_one = TRUE)

  ordered_elem_list = list()
  for(k in 1:length(valid_order_grid)) {
    ordered_elem_list[[valid_order_grid[k]]] = unordered_elem_list[[valid_order_grid[k]]]
  }

  my_experiments = expand.grid(ordered_elem_list)

  return(my_experiments)
}

# Realize the experiments by computing a summary_list for each
compute_summary_list = function(my_experiments, verbose = TRUE) {
  summary_list = list()
  for(k in 1:nrow(my_experiments)) {
    if(verbose) {
      print(paste0("Experiment ", k, "/", nrow(my_experiments)))
    }
    N = my_experiments$N[[k]]
    alpha = my_experiments$alpha[[k]]
    Tmax = my_experiments$Tmax[[k]]
    my_matrix = my_experiments$my_matrix[[k]]
    g = my_experiments$g[[k]]
    densitypes = my_experiments$densitypes[[k]]
    types = my_experiments$types[[k]]
    manifold = my_experiments$manifold[[k]]

    whichNULL = .check_valid_NalphaTmax(N, alpha, Tmax)
    if(whichNULL == "alpha") {
      alpha = get_alpha(N, Tmax)
    } else if(whichNULL == "N") {
      N = get_N(alpha, Tmax)
    } else if(whichNULL == "Tmax") {
      Tmax = get_Tmax(alpha, N)
    } else {
      stop("whichNULL should be 'alpha', 'N' or 'Tmax'")
    }

    Evolution = get_evol(N, my_matrix, g, densitypes, types, alpha, manifold)
    summary_list[[k]] = summary_func(Evolution, manifold, alpha)
  }
  return(summary_list)
}

# Simple filtering function
filter_summary_list = function(summary_list,
                               element = "velocity", position = "at the end is",
                               greater_than = 0.1, condition = "for at least one particle") {
  idx_keep = c()
  if(condition == "for at least one particle") {
    cond = any
  } else if(condition == "for all particles") { # "for all particles"
    cond = all
  } else {
    stop("'cond' argument must be 'for at least one particle' or 'for all particles'")
  }
  for(k in 1:length(summary_list)) {
    if(position == "at the end is") {
      idx = summary_list[[k]][["N"]]
    } else {
      stop("'position' argument must be 'at the end is'")
    }
    last_vect = summary_list[[k]][[element]][idx,]
    if(cond(last_vect > greater_than)) {
      idx_keep = c(idx_keep, k)
    }
  }
  if(length(idx_keep) == 0) {
    warning("No elements verify the condition")
  }
  summary_list = summary_list[idx_keep]
  return(summary_list)
}

# Plotting the experiments
multiplot_scent = function(my_experiments,
                           my_pos_xy = data.frame(pos_x = "time", pos_y = "velocity", stringsAsFactors = FALSE),
                           summary_list, config_for_plot = config_for_plot_func()) {
  if(!is.null(levels(my_pos_xy$pos_x))) {
    stop("my_pos_xy columns should not contain factors")
  }
  p_list = list()
  global_idx = 1
  for(i in 1:length(summary_list)) {
    for(idx_pos_xy in 1:nrow(my_pos_xy)) {
      # Plot experiment i with x/y-axis of idx_pos_xy
      p_list[[global_idx]] = plot_scent(x = my_pos_xy$pos_x[idx_pos_xy],
                                        y = my_pos_xy$pos_y[idx_pos_xy],
                                        summary_list[[i]],
                                        config_for_plot)
      if(!is.null(my_experiments$title[i])) {
        p_list[[global_idx]] = p_list[[global_idx]] + ggplot2::ggtitle(my_experiments$title[i])
      }
      global_idx = global_idx + 1
    }
  }
  return(p_list)
}

# Saving the experiments by row in pdf on multiple pages
ggsave_func = function(p_list, outfile = "multipage.pdf",
                       nrow = 2, ncol = 2,
                       width = 29.7, height = 21, units = c("cm"), dpi = 300) {
  ml = gridExtra::marrangeGrob(p_list, ncol, nrow,
                               layout_matrix = matrix(seq_len(nrow *ncol),
                                                      nrow = nrow, ncol = ncol, byrow = TRUE))
  ggplot2::ggsave(outfile, ml, dpi = dpi, width = width, height = height, units = units)
}
