move = function(particles, 
                steps = 100, 
                eps = 0.1, d, f,
                action_every = NA, action = NULL, ...) {
  positions = particles$positions
  types = particles$types

  # Moving multiple steps
  init = array(positions, 
               dim = c(dim(positions), 1))
  arrayout = init
  
  init_grad = array(NA,
                    dim = c(dim(positions), 1))
  arrayout_grad = init_grad
  
  for(k in 1:steps) {
    current = arrayout[,,dim(arrayout)[3]]
    to_append = move1(current, types, eps, d, f)
    arrayout = abind(arrayout, to_append$positions, along=3)
    arrayout_grad = abind(arrayout_grad, to_append$grad, along=3)
    if(!is.na(action_every)) {
      if(k %% action_every == 0) {
        action(k, arrayout, arrayout_grad, ...)
      }
    }
  }
  return(list("positions" = arrayout, "grad" = arrayout_grad))
}

action = function(k, arrayout, arrayout_grad, ...) {
  print(k)
  png(paste0(k, ".png"), 800, 800)
  evolution = list("positions" = arrayout, "grad" = arrayout_grad)
  print(plotting(evolution, axes = FALSE)) #+ xlim(0,1) + ylim(0,1))
  dev.off()
}

actionTorus = function(k, arrayout, arrayout_grad, ...) {
  print(k)
  png(paste0(k, ".png"), 800, 800)
  evolution = list("positions" = arrayout, "grad" = arrayout_grad)
  print(plotting(evolution, axes = FALSE, gridlim = 20) + xlim(0,20) + ylim(0,20))
  dev.off()
}