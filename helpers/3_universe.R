setClass("Universe", 
         representation(particles = "list",
                        dim = "integer",
                        bounds = "numeric",
                        sum_elem = "integer",
                        nb_particles = "integer",
                        f = "function",
                        Df = "function")
                        #mix = "function")#,
         #evolution = "data.table",
)

setMethod("initialize", signature = "Universe",
          ## Constructor
          definition = function(.Object,
                                m = list(),
                                sigma = list(),
                                type = list(),
                                bounds = numeric(0),
                                sum_elem = 0L,
                                density = "normal") {
            ## Initialize slots
            .Object@nb_particles = length(m)
            .Object@particles = list()
            if(.Object@nb_particles == 0L) {
              .Object@dim = 0L
            } else {
              .Object@dim = length(m[[1]])
              for(i in 1:.Object@nb_particles) {
                .Object@particles[[i]] = new("Particle", 
                                             m = m[[i]], 
                                             sigma = sigma[[i]], 
                                             type = type[[i]])
              }
            }
            .Object@bounds = bounds
            .Object@sum_elem = sum_elem
            if(density == "normal") {
              .Object@f = function(m, sigma) {
                if(length(m) != .Object@dim) {
                  stop(paste0("Need m of length ", .Object@dim))
                } else if(is.null(dim(sigma))) {
                  stop(paste0("Need sigma a matrix of dim ", .Object@dim, " x ", .Object@dim))
                }
                  else if(dim(sigma)[1] != .Object@dim) {
                  stop(paste0("Need sigma of dim ", .Object@dim, " x ", .Object@dim))
                } else if(dim(sigma)[2] != .Object@dim) {
                  stop(paste0("Need sigma of dim ", .Object@dim, " x ", .Object@dim))
                }
                f(m, sigma, bounds, sum_elem)
              }
              .Object@Df = function(m, sigma) {
                if(length(m) != .Object@dim) {
                  stop(paste0("Need m of length ", .Object@dim))
                } else if(is.null(dim(sigma))) {
                  stop(paste0("Need sigma a matrix of dim ", .Object@dim, " x ", .Object@dim))
                }
                else if(dim(sigma)[1] != .Object@dim) {
                  stop(paste0("Need sigma of dim ", .Object@dim, " x ", .Object@dim))
                } else if(dim(sigma)[2] != .Object@dim) {
                  stop(paste0("Need sigma of dim ", .Object@dim, " x ", .Object@dim))
                }
                Df(m, sigma, bounds, sum_elem)
              }
            } else {
              stop("Not done for non normal densities yet")
            }
            
            
            
            # .Object@mix = 

            ## Check validity
            if(!all(sapply(.Object@particles, 
                           function(p){length(p@m)}) == .Object@dim)) {
              stop("All `m` must have the same dimension")
            }
            if(!all(sapply(.Object@particles, 
                           function(p){length(p@type)}) == .Object@dim)) {
              stop("All `type`` must have the same dimension")
            }
            if(!all(sapply(.Object@particles, 
                           function(p){dim(p@sigma)[1]}) == .Object@dim)) {
              stop("All `sigma` must have the same dimension")
            }
            if(!all(sapply(.Object@particles, 
                           function(p){dim(p@sigma)[2]}) == .Object@dim)) {
              stop("All `sigma` must have the same dimension")
            }
            
            return(.Object)
          }
)

setGeneric("get_m", function(object) {
  standardGeneric("get_m")
})

setMethod("get_m", "Universe", function(object) {
  return(lapply(object@particles, function(p){p@m}))
})

setGeneric("get_sigma", function(object) {
  standardGeneric("get_sigma")
})

setMethod("get_sigma", "Universe", function(object) {
  return(lapply(object@particles, function(p){p@sigma}))
})

setGeneric("get_type", function(object) {
  standardGeneric("get_type")
})

setMethod("get_type", "Universe", function(object) {
  return(lapply(object@particles, function(p){p@type}))
})

setGeneric("mix", function(object) {
  standardGeneric("mix")
})

setMethod("mix", "Universe", function(object) {
  function(x) {
    m = get_m(object)
    sigma = get_sigma(object)
    N = object@nb_particles
    mean(
      sapply(1:N, function(p_idx){
        object@f(m[[p_idx]], sigma[[p_idx]])(x)
      })
    )}
})

setGeneric("push", function(object) {
  standardGeneric("push")
})

setMethod("push", "Universe", function(object) {
  # push = function(m, type, alpha, Df, sigma, bounds, sum_elem) {
  #   Dg = mix_func(Df, m, sigma, bounds, sum_elem)
  #   m = m + type * alpha * Dg(m)
  #   return(m)
  # }
  
  ## Todo: let push working in dimension d > 1
  # Compute new_m one particle per one particle, then form the list.
})

