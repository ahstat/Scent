# t = 0
# alpha = 0.1
# 
# get_next_t = function(t, alpha = 0.1, to_which = 1) {
#   x = t + seq(from = 0, to = to_which, length.out = 1001)
#   idx = which.min(abs(sapply(x, function(x){great_circle_distance(n_theta(t), n_theta(x))}) - alpha))
#   #print(idx)
#   if(idx == length(x)) {
#     print("Increase to_which")
#   } else if(idx == 1) {
#     print("Decrease to_which")
#   }
#   t_new = x[idx]
#   return(t_new)
# }
# 
# t = c(0)
# for(i in 1:1) {
#   print(i)
#   t = c(t, get_next_t(t[length(t)]))
# }
# 
# t

M = c(-0.5720396, -0.7630453, 0.3008865)
N = c(0.1018386, 0.8846557, -0.4549870)
Lambda = c(crossprod(M, N))
Auto = c(crossprod(M, M))
# t to find = 0.1713 = moving from M to N by a rotation of alpha = 0.1

t = 0.1712866

n_theta = function(t) {
  out = cos(t) * M + sin(t) * N
  out = out / c(norm_Eucl_vec(out))
  return(out)
}

n_theta(t)
M_rotated = c(-0.6494064, -0.7145232, 0.2602460)
great_circle_distance(M, M_rotated) # ok, 0.0999991
great_circle_distance(M, N) # 2.626379
great_circle_distance(M_rotated, N) # 2.526379, ok decreased by 0.1, so closer to N now

#### New

t0 = atan(- 1 / Lambda)
N_prim = cos(t0)*N + sin(t0)*M

c(crossprod(N_prim, N))
c(crossprod(N_prim, M))




#### Old

#t such that:
cos(alpha) - c(crossprod(M, M_rotated))
cos(alpha) - c(crossprod(M, n_theta(t)))
cos(alpha) - c(crossprod(M, (cos(t) * M + sin(t) * N) / c(norm_Eucl_vec((cos(t) * M + sin(t) * N)))))
cos(alpha) - c(crossprod(M, (cos(t) * M + sin(t) * N) / c(sqrt(crossprod(cos(t) * M + sin(t) * N)))))
cos(alpha) - c(crossprod(M, (cos(t) * M + sin(t) * N) / c(sqrt(crossprod(cos(t) * M + sin(t) * N)))))
cos(alpha) - (M[1]*cos(t) * M[1] + M[2]*cos(t) * M[2] + M[3]*cos(t) * M[3] + M[1]*sin(t) * N[1] + M[2]*sin(t) * N[2] + M[3]*sin(t) * N[3]) / c(sqrt(crossprod(cos(t) * M + sin(t) * N)))
cos(alpha) - (M[1]*cos(t) * M[1] + M[2]*cos(t) * M[2] + M[3]*cos(t) * M[3] + M[1]*sin(t) * N[1] + M[2]*sin(t) * N[2] + M[3]*sin(t) * N[3]) / sqrt((cos(t) * M[1] + sin(t) * N[1])*(cos(t) * M[1] + sin(t) * N[1]) + (cos(t) * M[2] + sin(t) * N[2])*(cos(t) * M[2] + sin(t) * N[2]) + (cos(t) * M[3] + sin(t) * N[3])*(cos(t) * M[3] + sin(t) * N[3]))
cos(alpha) - (M[1]*cos(t) * M[1] + M[2]*cos(t) * M[2] + M[3]*cos(t) * M[3] + M[1]*sin(t) * N[1] + M[2]*sin(t) * N[2] + M[3]*sin(t) * N[3]) / sqrt(cos(t)^2 * M[1]^2 + sin(t)^2 * N[1]^2 + 2*cos(t)*sin(t)*M[1]*N[1] + cos(t)^2 * M[2]^2 + sin(t)^2 * N[2]^2 + 2*cos(t)*sin(t)*M[2]*N[2] + cos(t)^2 * M[3]^2 + sin(t)^2 * N[3]^2 + 2*cos(t)*sin(t)*M[3]*N[3])
cos(alpha) - (M[1]*cos(t) * M[1] + M[2]*cos(t) * M[2] + M[3]*cos(t) * M[3] + M[1]*sin(t) * N[1] + M[2]*sin(t) * N[2] + M[3]*sin(t) * N[3]) / sqrt(1 + 2*cos(t)*sin(t)*Lambda)
cos(alpha) - (M[1]^2 *cos(t) + M[2]^2 *cos(t) + M[3]^2 *cos(t) + Lambda * sin(t)) / sqrt(1 + 2*cos(t)*sin(t)*Lambda)
cos(alpha) - (Auto * cos(t) + Lambda * sin(t)) / sqrt(1 + 2 * cos(t) * sin(t) * Lambda)
(1 + 2 * cos(t) * sin(t) * Lambda) * cos(alpha)^2 - (Auto * cos(t) + Lambda * sin(t))^2
(1 + 2 * cos(t) * sin(t) * Lambda) * cos(alpha)^2 - Auto^2 * cos(t)^2 - Lambda^2 * sin(t)^2 - 2 * Auto * Lambda * cos(t) * sin(t)
cos(alpha)^2 + 2 * cos(t) * sin(t) * Lambda * cos(alpha)^2 - Auto^2 * cos(t)^2 - Lambda^2 * sin(t)^2 - 2 * Auto * Lambda * cos(t) * sin(t)
cos(alpha)^2 + 2 * cos(t) * sin(t) * Lambda * (cos(alpha)^2 - Auto) - Auto^2 * cos(t)^2 - Lambda^2 * sin(t)^2 #ok
cos(alpha)^2 + 2 * cos(t) * sin(t) * Lambda * cos(alpha)^2 - (cos(t) * Auto + sin(t) * Lambda)^2
#cos(alpha)^2 + sin(2*t) * Lambda * cos(alpha)^2 - (cos(t) * Auto + sin(t) * Lambda)^2
C1 = Lambda * (cos(alpha)^2 - Auto)
C2 = cos(alpha)^2
C2 + sin(2*t) * C1 - (Auto^2 * cos(t)^2 + Lambda^2 - Lambda^2 * cos(t)^2)
C3 = (Auto^2 - Lambda^2)
C4 = Lambda^2
C2 + sin(2*t) * C1 - C3 * cos(t)^2 - C4



C2 - C4 - C3 * cos(t)^2 + C1 * 2 * sin(t) * cos(t)

a = tan(t/2)

C2 - C4 + cos(t) * (- C3 * cos(t) + C1 * 2 * sin(t))


C2 - C4 + ((1-a^2) / (1+a^2)) * (- C3 * ((1-a^2) / (1+a^2)) + C1 * 2 * (2*a)/(1+a^2))


- C3 + C3 * a^2 + C1 * 4 * a + C3 * a^2 - C3 * a^4 - C1 * 4 *a^3 - (C4 - C2) * (1 + a^4 + 2 * a^2)



- C3 + C3 * a^2 + C1 * 4 * a + C3 * a^2 - C3 * a^4 - C1 * 4 *a^3 - (C4 - C2) - (C4 - C2) * a^4 - (C4 - C2) * 2 * a^2


(C3 + C4 - C2) +
  -1 * C1 * 4 * a +
  -2 * (C3 - C4 + C2) * a^2 +
   C1 * 4 *a^3 +
  (C3 + C4 - C2) * a^4
  
(C3 + C4 - C2) * (1 + a^4) + 
  -1 * C1 * 4 * a * (1-a^2) +
  2 * (-C3 + C4 - C2) * a^2

# But Auto = 1 so C3+C4 = 1...

(1 - C2) * (1 + a^4) + 
  -1 * C1 * 4 * a * (1-a^2) +
  2 * (2 * Lambda^2 - 2 + 1 - C2) * a^2

(sin(alpha)^2) * (1 + a^4) + 
  Lambda * (sin(alpha)^2) * 4 * a * (1-a^2) +
  2 * (2 * (Lambda^2 - 1) + sin(alpha)^2) * a^2




# Reproduce line 61
cos(alpha)^2 + 2 * cos(t) * sin(t) * Lambda * cos(alpha)^2 - (cos(t) * Auto + sin(t) * Lambda)^2
cos(alpha)^2 + 2 * cos(t) * sin(t) * Lambda * cos(alpha)^2 - (cos(t) + sin(t) * Lambda)^2 #( = 0)

a = tan(t/2)

cos(alpha)^2 + 4 * (a)*(1-a)*(1+a)/(1+a^2)^2 * Lambda * cos(alpha)^2 - ((1-a)*(1+a) + 2*a * Lambda)^2 / (1+a^2)^2
(1+a^2)^2 * cos(alpha)^2 + 4 * a*(1-a)*(1+a) * Lambda * cos(alpha)^2 - ((1-a)*(1+a) + 2*a * Lambda)^2
(1+a^2)^2 * cos(alpha)^2 + 4 * a*(1-a)*(1+a) * Lambda * cos(alpha)^2 - ((1-a)*(1+a) + 2*a * Lambda)^2
(1+a^2)^2 * cos(alpha)^2 + 4 * a*(1-a)*(1+a) * Lambda * cos(alpha)^2 - (1-a)^2*(1+a)^2 - 4*a^2 * Lambda^2 - 4 * (1-a)*(1+a)*a * Lambda
(1+a^2)^2 * cos(alpha)^2 + 4 * a*(1-a)*(1+a) * Lambda * (cos(alpha)^2 - 1) - (1-a)^2*(1+a)^2 - 4*a^2 * Lambda^2



(1+a^2)^2 * cos(alpha)^2 + 
  4 * a*(1-a)*(1+a) * Lambda * (cos(alpha)^2 - 1) - 
  (1-a)^2*(1+a)^2 - 
  4*a^2 * Lambda^2



R = cos(alpha)^2

(1 + a^4 + 2*a^2) * R + 
  4 * a*(1-a^2) * Lambda * (R - 1) - 
  1 - a^4 + 2*a^2 - 
  4*a^2 * Lambda^2

(R - 1) + 
  4 * Lambda * (R - 1) * a +
  2 * (2 - 2 * Lambda^2 + R - 1) * a^2 -
  4 * Lambda * (R - 1) * a^3 +
  (R - 1) * a^4
  
Q = -sin(alpha)^2

Q + 
  4 * Lambda * Q * a +
  2 * (2 - 2 * Lambda^2 + Q) * a^2 -
  4 * Lambda * Q * a^3 +
  Q * a^4



1 + 4 * Lambda * a + 2 * a^2 - 4 * Lambda * a^3 + a^4 + 
  4 * (1 - Lambda^2) * a^2 / Q

1 + 4 * Lambda * a - 4 * Lambda * a^3 + a^4 + 2 * a^2 +
  4 * (1 - Lambda^2) * a^2 / Q

1 + 4 * Lambda * a + (4*Lambda^2-2)*a^2 - 4 * Lambda * a^3 + a^4 + 2 * a^2 - (4*Lambda^2-2)*a^2 + 
  4 * (1 - Lambda^2) * a^2 / Q

1 + 4 * Lambda * a + (4*Lambda^2-2)*a^2 - 4 * Lambda * a^3 + a^4 - 4*(Lambda^2-1)*a^2 + 
  4 * (1 - Lambda^2) * a^2 / Q

# (a^2 - 2*a*L - 1)^2 = 
#   1 + 4*L*a + (4*L^2 - 2)*a^2 - 4*a^3*L + a^4

(a^2 - 2*a*Lambda -1)^2 - 4*(Lambda^2-1)*a^2 + 
  4 * (1 - Lambda^2) * a^2 / Q

(a^2 - 2*a*Lambda -1)^2 + 4*(1 - Lambda^2)*a^2 + 
  4 * (1 - Lambda^2) * a^2 / Q

(a^2 - 2*a*Lambda -1)^2 + 
  4 * (1 - Lambda^2) * a^2 * (1 + 1/ Q)

(a - 2*Lambda - 1/a)^2 + 4 * (1 - Lambda^2) * (1 + 1/ Q)

(a - 2*Lambda - 1/a)^2 == -4 * (1 - Lambda^2) * (1 + 1/ Q)

a - 1/a + sqrt(-4 * (1 - Lambda^2) * (1 + 1/ Q)) - 2*Lambda

a - 1/a + sqrt(-4 * (1 - Lambda^2) * (1 + 1/ Q)) - 2*Lambda

D = sqrt(-4 * (1 - Lambda^2) * (1 + 1/ Q)) - 2*Lambda

a - 1/a + D

Delta = D^2 + 4

(-D + sqrt(Delta)) / 2 - a # :)
(or : (-D - sqrt(Delta)) / 2)



2 * atan(a) - t

2 * atan((-D + sqrt(Delta)) / 2) - t

2 * atan((-sqrt(-4 * (1 - Lambda^2) * (1 + 1/ Q)) + 2*Lambda + sqrt((sqrt(-4 * (1 - Lambda^2) * (1 - 1/ sin(alpha)^2)) - 2*Lambda)^2 + 4)) / 2) - t

# Lambda = <x|y>
# alpha = angle we want
# output: t = the parameter to take with sin and cos
2*atan((-sqrt(-4 *(1 - Lambda^2) * (1 - 1/ sin(alpha)^2)) + 2*Lambda + sqrt((sqrt(-4 * (1 - Lambda^2) * (1 - 1/sin(alpha)^2)) - 2*Lambda)^2 + 4)) / 2) - t



# Conclusion:
Lambda = c(crossprod(M, N))
Q = -sin(alpha)^2
D = sqrt(-4 * (1 - Lambda^2) * (1 + 1/ Q)) - 2*Lambda
Delta = D^2 + 4

2 * atan((-D + sqrt(Delta)) / 2) - t
# or 2 * atan((-D - sqrt(Delta)) / 2)


2 * (pi/2 - atan(2/ (-D + sqrt(D^2 + 4)))) - t
2 * (pi/2 - atan((2/D)/ (-1 + sqrt(1 + 4/D^2)))) - t
2 * (pi/2 - atan((2/D)/ (-1 + sqrt(1 + 4/D^2)))) - t
2 * (pi/2 - atan(-(2/D)/ (1 - sqrt(1 + 4/D^2)))) - t


p = 2/D
2 * (pi/2 - atan(-p/ (1 - sqrt(1 + p^2)))) - t

2 * atan(-p/ (1 - sqrt(1 + p^2)))
2 * atan(-p/ (1 + sqrt(1 + p^2)))
-atan(p)

# With ln?
2 * atan((-D + sqrt(Delta)) / 2) - t
2 * atan((-D + sqrt(Delta)) / 2) - t

O = (-D + sqrt(Delta)) / 2
2 * Re(log((1 + 1i * O) / (1 - 1i * O))/(2*1i)) - t

2 * Re(log((2 + 1i * (-D + sqrt(Delta))) / (2 - 1i * (-D + sqrt(Delta))))/(2*1i)) - t









