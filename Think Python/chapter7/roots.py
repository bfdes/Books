# Ex 7.1
def sqrt(a, estimate, epsilon=0.00001):
  x = estimate
  while True:
    y = (x + a / x) / 2
    if(abs(y-x) < epsilon):
      break
    x = y
  return y
