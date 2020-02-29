matrix_of_distances = function(my_matrix) {
  M = matrix(NA, nrow = nrow(my_matrix), ncol = nrow(my_matrix))
  for(i in 1:(nrow(my_matrix)-1)) {
    for(j in (i+1):nrow(my_matrix)) {
      M[i, j] = great_circle_distance(my_matrix[i,], my_matrix[j,])
    }
  }
  return(M)
}

dist_evol = function(Evolution) {
  n_elem = dim(Evolution)[1]
  N = dim(Evolution)[3]
  distEvolution = array(NA, dim = c(n_elem, n_elem, N))
  for(i in 1:N) {
    distEvolution[,,i] = matrix_of_distances(Evolution[,,i])
  }
  return(distEvolution)
}

plot_dist_evol = function(distEvolution, main_title = "") {
  n_elem = dim(distEvolution)[1]
  par(mfrow=c(n_elem, n_elem), oma = c(5,4,0,0) + 1.5, mar = c(0,0,1,1) + 0.5)
  for(i in 1:nrow(distEvolution)) {
    for(j in 1:nrow(distEvolution)) {
      #print(paste(i, j))
      if(!all(is.na(distEvolution[i,j,]))) {
        plot(distEvolution[i,j,], 
             type = "l", 
             #xaxt='n', 
             main = paste0("(", i, ",", j, ")"),
             ylim = c(0-1e-8, pi+1e-8))
      } else {
        plot.new()
      }
    }
  }
  title(main_title, outer = TRUE)
  par(mfrow=c(1,1))
}

######################################
# Loop over all types and densitypes #
######################################
combin_types = function(n_elem, unitype = c(-1, 1)) {
  out = expand.grid(rep(list(unitype), n_elem))
  return(out)
}

combin = function(n_elem, types = c("type", "densitype"), unitype = c(-1, 1)) {
  if(length(types) == 2) {
    vectypes = combin_types(2*n_elem)
    colnames(vectypes) = apply(expand.grid(c("type", "densitype"), 1:n_elem), 1, paste, collapse = "")
  } else if(length(types) == 1) {
    if(types == "type") {
      densitypes = rep(+1, n_elem)
      vectypes = combin_types(n_elem)
      colnames(vectypes) = apply(expand.grid(c("type"), 1:n_elem), 1, paste, collapse = "")
      for(i in 1:n_elem) {
        vectypes[[paste0("densitype", i)]] = densitypes[i]
      }
      # reorder
      my_order = apply(expand.grid(c(0, n_elem), 1:n_elem), 1, sum)
      vectypes = vectypes[,my_order]
    } else if(types == "densitype") {
      types = rep(+1, n_elem)
      vectypes = combin_types(n_elem)
      colnames(vectypes) = apply(expand.grid(c("densitype"), 1:n_elem), 1, paste, collapse = "")
      for(i in 1:n_elem) {
        vectypes[[paste0("type", i)]] = types[i]
      }
      # reorder
      my_order = apply(expand.grid(c(n_elem, 0), 1:n_elem), 1, sum)
      vectypes = vectypes[,my_order]
    } else {
      stop("If length(types) == 1, only 'type' and 'densitype' is possible")
    }
  } else {
    stop("length(types) must be of length 1 or 2")
  }
  colnames(vectypes) = gsub(" ", "", colnames(vectypes))
  return(vectypes)
}

evol = function(my_matrix, i, vectypes, N, Df, alpha = 0.1) {
  types = as.numeric(vectypes[i, c(paste0("type", 1:nrow(my_matrix)))])
  densitypes = as.numeric(vectypes[i, c(paste0("densitype", 1:nrow(my_matrix)))])
  Evolution = get_evol(my_matrix, N, Df, densitypes, types, alpha)
  return(Evolution)
}

evol_and_plot = function(my_matrix,
                         i, vectypes, N, alpha = 0.1, 
                         plot_dist = TRUE, plot_evol = TRUE, plot_evol_proj = FALSE,
                         savepng = FALSE) {
  Evolution = evol(my_matrix, i, vectypes, N, Df, alpha)
  distEvolution = dist_evol(Evolution)
  
  print(paste0(i, "/", nrow(vectypes)))
  
  if(plot_dist) {
    if(savepng) {
      png(paste0("plots/", i, ".png"), width = 1500, height = 1500)
      plot_dist_evol(distEvolution, main_title = paste0("(", paste(vectypes[i,], collapse = " "), ")"))
      dev.off()
    } else {
      plot_dist_evol(distEvolution, main_title = paste0("(", paste(vectypes[i,], collapse = " "), ")"))
    }
  }
  
  if(plot_evol) {
    step_min = max(N/2, N - 100)
    step_max = N
    
    if(savepng) {
      png(paste0("plots/", i, ".png"))
      main = i #paste0(names(vectypes[i,]), ": ", vectypes[i,], " / ", collapse = "")
      plot_evolution(Evolution, step_min = step_min, step_max = step_min, main = main)
      dev.off()
    } else {
      main = i #paste0(names(vectypes[i,]), ": ", vectypes[i,], " / ", collapse = "")
      plot_evolution(Evolution, step_min = step_min, step_max = step_min, main = main)
    }
  }
  
  if(plot_evol_proj) {
    print("Not done for now. It corresponds to long/lat with S^2 and a segment in S^1")
    # tt = 1
    # plot_evolution(Evolution, step_min = tt, step_max = tt+10, main = main); tt = tt+1000
    # source("misc/misc_S2_to_latlong.R")
    # my_matrix_converted = t(apply(my_matrix, 1, latlong_func))
  }
}

pseudo_random_number = function() {
  as.integer((as.double(Sys.time()) * 1000 + Sys.getpid()) %% 2^31)
}

rspin = function(n_elem) {
  2 * rbinom(n_elem, 1, prob = 1/2) - 1
}

approx_velocity = function(Evolution) {
  # velocity on R^n (not on S^n), ok for seeing convergence or not
  N = dim(Evolution)[3]
  velocity = rep(NA, N)
  for(i in 2:N) {
    velocity[i] = sum(apply(Evolution[,,i] - Evolution[,,i-1], 1, function(x){sqrt(sum(x^2, na.rm = TRUE))}))
  }
  return(velocity)
}
