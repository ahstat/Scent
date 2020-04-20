library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

convert_to_list = function(x, normal_class, allow_null = FALSE, normally_size_one = FALSE) {
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

check_valid_NalphaTmax = function(N, alpha, Tmax) {
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
    stop("error in check_valid_NalphaTmax")
  }
}

order_grid_default_func = function() {
  c("manifold", "my_matrix",
    "g", "densitypes", "types",
    "N", "alpha", "Tmax",
    "pos_x", "pos_y")
}

check_valid_order_grid = function(order_grid) {
  order_grid_default = order_grid_default_func()
  if(max(rle(sort(order_grid))[[1]]) > 1) {
    stop("Some repeated elements in order_grid")
  }
  if(!all(order_grid %in% order_grid_default)) {
    stop("order_grid must contains elements of ", paste(order_grid_default, collapse = ", "))
  }
  return(c(order_grid, setdiff(order_grid_default, order_grid)))
}

define_experiments = function(my_matrix, g, densitypes, types, manifold,
                              N = NULL, alpha = NULL, Tmax = NULL,
                              pos_x, pos_y,
                              order_grid = order_grid_default_func()) {
  if(is.null(order_grid)) {
    order_grid = order_grid_default_func()
  }
  whichNULL = check_valid_NalphaTmax(N, alpha, Tmax)
  valid_order_grid = check_valid_order_grid(order_grid)

  unordered_elem_list = list()
  unordered_elem_list[["manifold"]] = convert_to_list(manifold, "character", normally_size_one = TRUE)
  unordered_elem_list[["my_matrix"]] = convert_to_list(my_matrix, c("matrix"))
  unordered_elem_list[["g"]] = convert_to_list(g, c("function"))
  unordered_elem_list[["densitypes"]] = convert_to_list(densitypes, "numeric", normally_size_one = FALSE)
  unordered_elem_list[["types"]] = convert_to_list(types, "numeric", normally_size_one = FALSE)
  unordered_elem_list[["N"]] = convert_to_list(N, c("numeric", "integer"), allow_null = TRUE, normally_size_one = TRUE)
  unordered_elem_list[["alpha"]] = convert_to_list(alpha, "numeric", allow_null = TRUE, normally_size_one = TRUE)
  unordered_elem_list[["Tmax"]] = convert_to_list(Tmax, "numeric", allow_null = TRUE, normally_size_one = TRUE)
  unordered_elem_list[["pos_x"]] = convert_to_list(pos_x, "character", normally_size_one = TRUE)
  unordered_elem_list[["pos_y"]] = convert_to_list(pos_y, "character", normally_size_one = TRUE)

  ordered_elem_list = list()
  for(k in 1:length(valid_order_grid)) {
    ordered_elem_list[[valid_order_grid[k]]] = unordered_elem_list[[valid_order_grid[k]]]
  }

  my_experiments = expand.grid(ordered_elem_list)

  return(my_experiments)
}

####

my_matrix = unif_on_S1(5)
g = g_sin
densitypes = list(c(1, 1, 1, 1, 1),
                  c(1, -1, 1, 1, 1))
types = c(-1, -1, 1, 1, 1)
manifold = "E"
N = c(10, 100, 1000)
Tmax = 3
alpha = NULL

pos_x = "velocity"
pos_y = "acceleration"

order_grid = c("N", "alpha")
#order_grid = order_grid_default_func()

# Make the whole data frame of experiments
define_experiments(my_matrix, g, densitypes, types, manifold,
                   N, alpha, Tmax,
                   pos_x, pos_y,
                   order_grid)

# TODO (after TODO next): types and densitypes over all possibilities as a list of vectors

#### TODO next:

summary_list = list()
Tmax = 3

for(k in 1:length(Ns)) {
  N = Ns[k]
  Evolution = get_evol(N, my_matrix, g, densitypes, types, get_alpha(N, Tmax), manifold)
  summary_list[[k]] = summary_func(Evolution, manifold, get_alpha(N, Tmax))
}

index = c(1, 2, 3)
pos_x = c("velocity", "velocity", "velocity")
pos_y = c("acceleration", "acceleration", "acceleration")
subplot_names = c("1", "fff", "23")

plot_config = plot_config_func(plotting_option = 1,
                               t_labels = FALSE,
                               kind_of_palette = "default")

if(length(index) != length(summary_list)) {
  stop("index and summary_list must have same length")
}
if(length(pos_x) != length(summary_list)) {
  stop("pos_x and summary_list must have same length")
}
if(length(pos_y) != length(summary_list)) {
  stop("pos_y and summary_list must have same length")
}

p_list = list()
for(i in 1:length(summary_list)) {
  p_list[[i]] = plot_scent(x = pos_x[[i]], y = pos_y[[i]], summary_list[[index[i]]], plot_config) +
    ggplot2::ggtitle(paste0("N=", Ns[i])) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
}




ml <- marrangeGrob(p_list, nrow=2, ncol=2)
## non-interactive use, multipage pdf
ggsave("multipage.pdf", ml)
## interactive use; calling `dev.new` multiple times
ml

