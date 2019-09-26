# * What is the object `positions`:
# It is a matrix describing the position of each particle.
# It has a dimension nb_particles x space_nb_dimension
# Ex: particle 1 is positions[1,] a vector with position in each
# direction (as a number)
# Ex: With four particles in dimension two
# positions = matrix(c(0,     0,
#                      0.5,   0,
#                      0,   0.5,
#                      0.5, 0.5), nrow = 4, ncol = 2, byrow = TRUE)

# * What is the object `types`:
# It is the behavior -1 (repulsing) or 1 (attracting) of the particles

init_4_sinmove = function(a = 0.5, b = 0.5) {
  # a = 0.5
  # b = 1.8121475245 # default = 1.9 / 2 = no effect / 
  #  <= 1.8121475244 = inner too much so blocked
  #  >= 1.8121475245 = ok change direction
  positions = matrix(c(0, 0,
                       a, 0,
                       0, b,
                       a, b), nrow = 4, ncol = 2, byrow = TRUE)
  types = c(1, -1, 1, -1)
  particles = list("positions" = positions, "types" = types)
  return(particles)
}
