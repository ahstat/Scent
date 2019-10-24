#########################################################
# Distance between positions on R^n with Euclidian norm #
#########################################################

##
# Function
##
dist_between_positions.real = function(positions) {
  ## For Euclidian norm, it is quicker to use rdist
  return(rdist(positions)) # https://stackoverflow.com/questions/9879608
}

##
# Example
##
positions = matrix(c(0, 0,
                     1, 1,
                     0, 1),
                   ncol = 2, byrow = T)
dist_between_positions.real(positions)
rm(positions)
# We obtain the matrix M:
#                 0 sqrt(2)       1 
#           sqrt(2)       0       1
#                 1       1       0
# M[1,2] is the distance between position (0,0) and position (1,1)
# M[3,1] is the distance between position (0,1) and position (0,0).
# Etc.

##
# Unit tests
##
positions2d = matrix(c(0.9, 0.9,
                       0.1, 0.2,
                       0.0, 0.0,
                       9.9, -9.1),
                     ncol = 2, byrow = T)
checkEqualsNumeric(dist_between_positions.real(positions2d),
                   matrix(c(0, sqrt(0.8^2+0.7^2), sqrt(2*0.9^2), sqrt(9^2 + (0.9+9.1)^2),
                            sqrt(0.8^2+0.7^2), 0, sqrt(0.1^2+0.2^2), sqrt(9.8^2+9.3^2),
                            sqrt(2*0.9^2), sqrt(0.1^2+0.2^2), 0, sqrt(9.9^2+9.1^2),
                            sqrt(9^2 + (0.9+9.1)^2), sqrt(9.8^2+9.3^2), sqrt(9.9^2+9.1^2), 0), 
                          ncol = 4, byrow = TRUE))
rm(positions2d)
positions3d = matrix(c(0, 0, 0,
                       -0.1, 0.1, 0.9),
                     ncol = 3, byrow = T)
checkEqualsNumeric(dist_between_positions.real(positions3d),
                   matrix(c(0, sqrt(0.1^2+0.1^2+0.9^2), 
                            sqrt(0.1^2+0.1^2+0.9^2), 0),
                          ncol = 2, byrow = TRUE))
rm(positions3d)

#################################################
# Normalized direction between positions on R^n #
#################################################

##
# Helper: Normalized direction from x to y
##
normal.real = function(i, j, positions, dist_between_positions) {
  x = positions[i, ]
  y = positions[j, ]
  (y-x) / dist_between_positions[i, j]
}

##
# Function
##
normal_between_positions.real = function(positions, dist_between_positions) {
  nb_part = nrow(positions)
  n_positions = array(NA, dim = c(nb_part, nb_part, ncol(positions)))
  for(i in 1:nb_part) {
    for(j in 1:nb_part) {
      n_positions[i,j,] = normal.real(i, j, positions, dist_between_positions)
    }
  }
  return(n_positions)
}

##
# Example
##
positions = matrix(c(0, 0,
                     1, 1,
                     0, 1),
                   ncol = 2, byrow = T)
normal_between_positions.real(positions, 
                              dist_between_positions.real(positions))
# We obtain the array A:
#                  (NaN, NaN) (sqrt(2)/2, sqrt(2)/2) (0, 1)
#    (-sqrt(2)/2, -sqrt(2)/2)             (NaN, NaN) (-1, 0)
#                     (0, -1)                 (1, 0) (NaN, NaN)
# A[1,2,] is the normalized direction from position 1 to position 2
# A[3,2,] is the normalized direction from position 3 to position 2
# Etc.

##
# Unit tests
##

## Check for a simple example
positions = matrix(c(0, 0,
                     1, 1,
                     0, 1),
                   ncol = 2, byrow = T)
checkEqualsNumeric(normal_between_positions.real(positions, dist_between_positions.real(positions)),
                   array(c(NaN, -sqrt(2)/2,  0, 
                           sqrt(2)/2, NaN, 1,
                           0, -1, NaN,
                           NaN, -sqrt(2)/2, -1,
                           sqrt(2)/2, NaN, 0,
                           1, 0, NaN), dim = c(3, 3, 2)))
rm(positions)

## Check it is normalized
positions5d = matrix(c(0.9, 0.9, 0.4, 0.4, 0.0,
                       0.1, 0.2, 0.0, 0.1, 0.0,
                       0.0, 0.0, 0.0, 0.0, 0.0,
                       9.9, -9.1, -0.5, -0.6, 0.0),
                     ncol = 5, byrow = T)
A5 = normal_between_positions.real(positions5d, 
                                  dist_between_positions.real(positions5d))
N_A5 = apply(A5^2, c(1,2), sum)
diag(N_A5) = 1
checkEqualsNumeric(N_A5, matrix(rep(1, 16), ncol = 4, nrow = 4))
rm(positions5d, A5, N_A5)
