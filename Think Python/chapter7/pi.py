# Ex 7.3
from math import sqrt, factorial, pi

# The implementation of factorial in the standard lib should be stack-safe.

def estimate_pi(epsilon=1e-15):
  sum = 0
  k = 0
  while True:
    term = (2*sqrt(2) / 9801) * (factorial(4*k)*(1103 + 26390*k)) / (factorial(k)**4 * 396**(4*k))
    if(term < epsilon):
      break
    sum += term
    k += 1
  return sum
