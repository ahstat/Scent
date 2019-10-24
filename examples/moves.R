#########
# Moves #
#########
move_4_particles_1 = function() {
  d = d_eucl
  f = f_approxderivdnorm # f_derivdnorm
  eps = 0.1
  particles = init_4_sinmove(a=0.5, b=1.9)
  steps = 200
  
  evolution = move(particles, steps, eps, d, f)
  df = convert_array_to_df(evolution$positions)
  df$alpha = (df$iteration - 1)/max(df$iteration) # for the moving effect

  print(plotting_df(df, axes = TRUE))
  return("Demo 1 finished")
}

move_4_particles_2 = function() {
  d = d_eucl
  f = f_derivdnorm
  eps = 0.1
  particles = init_4_sinmove(a=0.1, b=2.1)
  steps = 200
  
  evolution = move(particles, steps, eps, d, f)
  df = convert_array_to_df(evolution$positions)
  df$alpha = (df$iteration - 1)/max(df$iteration) # for the moving effect
  
  print(plotting_df(df, axes = TRUE))
  return("Demo 2 finished")
}
