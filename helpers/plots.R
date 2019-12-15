plot_evolution = function(Evolution, step_min = 1, step_max = NA, ...) {
  if(dim(Evolution)[2] == 2) {
    plot_evolution_circle(Evolution, step_min, step_max, ...)
  } else if(dim(Evolution)[2] == 3) {
    plot_evolution_sphere(Evolution, step_min, step_max)
  } else {
    stop(paste0("Evolution should live on S1 or S2, but found: S", dim(Evolution)[2]-1))
  }
}

######
# 2D #
######
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

######
# 3D #
######
plot_evolution_sphere = function(Evolution, step_min, step_max) {
  if(is.na(step_max)) {
    step_max = dim(Evolution)[3]
  }
  
  plot_sphere()
  for(step in seq(from = step_min, to = step_max)){
    step = floor(step)
    plot_point(Evolution[1,,step], "red")
    plot_point(Evolution[2,,step], "blue")
    plot_point(Evolution[3,,step], "green")
    plot_point(Evolution[4,,step], "yellow")
  }
}
