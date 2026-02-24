library(hypergeo)

# The example below currently works fine for integer k. For non-integer k, we require summation till infinity, and it seems to not converge.

k=4
s=2
U = 1/s
L = 1 + 1/s

sum=0
for (j in 0:k){
  z = 1i*pi*(k-2*j)
  sum = sum + (-1)^j * choose(k,j) * genhypergeo(U, L, z)
}
Re(sum/(2i)^k)


sinuarea(s,k)
