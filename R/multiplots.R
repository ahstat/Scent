library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

my_matrix = unif_on_S1(5)
g = g_sin
densitypes = c(1, 1, 1, 1, 1)
types = c(-1, -1, 1, 1, 1)
manifold = "E"


on_N
on_alpha
on_Tmax
on_types
on_g
on_nbpart
on_posx
on_posy


# Make the whole data frame of experiments:

# N / my_matrix / g / densitypes / types / alpha / manifold / pos_x / pos_y / subplot_names

if(class(my_matrix) == "matrix") {
  len_my_matrix = 1
} else if(class(my_matrix) == "list") {
  len_my_matrix = length(my_matrix)
} else {
  stop("my_matrix must be either a matrix or a list")
}

nb_rows = length(N) * length(my_matrix) * length(g) *
  length(densitypes) * length(types) * length(alpha) *
  length(manifold) * length(pos_x) * length(pos_y) * length(subplot_names)


Ns = c(10, 100, 1000)

dd = data.frame(N = Ns,
                my_matrix = NA)
dd$my_matrix[1] = list(my_matrix)

pos_x = c("velocity", "velocity", "velocity")
pos_y = c("acceleration", "acceleration", "acceleration")
subplot_names = c("1", "fff", "23")


get_evol(N, my_matrix, g, densitypes, types, get_alpha(N, Tmax), manifold)




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

