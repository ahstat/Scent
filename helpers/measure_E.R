########
# Misc #
########
norm_Eucl_vec = function(A) {
  # Return ||A|| of a point of R^d
  # https://stackoverflow.com/questions/10933945/
  sqrt(crossprod(A))
}

####################################
# Distance between two points on E #
####################################
distance_E = function(A, B) {
  c(norm_Eucl_vec(B - A))
}

#######
# Log #
#######
Log_weighted_E = function(A, B, Df) {
  theta = distance_E(A, B)
  # B_weighted_on_tanA = (B - A) * (Df(theta) / theta)
  B_weighted_on_tanA = Df(theta) * (B - A) / theta
  return(B_weighted_on_tanA)
}

#######
# Exp #
#######
Exp_E = function(A, B_on_tanA) {
  return(A + B_on_tanA)
}
