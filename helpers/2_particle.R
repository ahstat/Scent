setClass("Particle", 
         representation(m = "numeric",
                        sigma = "matrix",
                        type = "numeric",
                        dim = "integer")
)

setMethod("initialize", signature = "Particle",
          ## Constructor
          definition = function(.Object,
                                m = c(0,0),
                                sigma = diag(2),
                                type = c(-1, 1)) {
            ## Initialize slots
            .Object@m = m
            .Object@type = type
            .Object@dim = length(m)
            if(.Object@dim == 1 & length(sigma) == 1) {
              sigma = as.matrix(sigma)
            }
            .Object@sigma = sigma
            
            ## Check validity
            dim = .Object@dim
            if(!all(dim(.Object@sigma) == c(dim, dim))) {
              stop(paste0("sigma must be a matrix of size ", dim, "x", dim))
            }
            if(length(.Object@type) != dim) {
              stop(paste0("length of type must be ", dim))
            }
            
            return(.Object)
          }
)
