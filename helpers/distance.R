################################
# Define distance in our space #
################################
d_eucl = function(x, y) {
  # Distance between x and y (two individual positions of the space)
  sqrt(sum((x - y)^2))
}
