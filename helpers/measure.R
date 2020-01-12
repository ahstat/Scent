Log_weighted_M = function(manifold = "S") {
  if(manifold == "E") {
    return(Log_weighted_E)
  } else if(manifold == "S") {
    return(Log_weighted_S)
  } else_if(manifold == "H") {
    return(Log_weighted_H)
  }
}

Exp_M = function(manifold = "S") {
  if(manifold == "E") {
    return(Exp_E)
  } else if(manifold == "S") {
    return(Exp_S)
  } else_if(manifold == "H") {
    return(Exp_H)
  }
}

dist_M = function(A, B, manifold = "S") {
  if(manifold == "E") {
    return(distance_E(A, B))
  } else if(manifold == "S") {
    return(distance_S_great_circle(A, B))
  } else_if(manifold == "H") {
    return(distance_H(A, B))
  }
}
