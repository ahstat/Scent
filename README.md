# Scent

# Riding on oneself

### Blabla for Df function
If density is C + cos(t), then we get -sin(t)
We select C such that we have a density over the hypersphere
If we have a sphere S(n), 
We integrate from a point A to the opposite pole B, and t is the
parallel from 0 (in A) to pi (in B). For pi/2, we are on the equator,
of size Vol(S(n-1))*1. If we are between equator and pole, we multiply
by sin(t). Since f(x) = f(t) [same value over all directions],
we want f such that Vol(S(n-1))*int_0^\pi f(t) sin(t) dt is 1

In our example, int_0^\pi cos(t) sin(t) dt = 0 so
Vol(S(n-1))*int_0^\pi f(t) sin(t) dt = Vol(S(n-1)) * C * int_0^\pi sin(t) dt
                                     = Vol(S(n-1)) * C * 2 = 1
So: C = 1 / (4 * Vol(S(n-1))) = Gamma(n/2) / (4*pi^(n/2))

t = seq(from = -pi, to = pi, length.out = 100)
plot(t, 1 + cos(t), ylim = c(-1, 2))
lines(t, -sin(t), col = "red")

Maybe more natural densities exist
