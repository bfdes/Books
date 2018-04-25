# Ex 6.5
def gcd(a: int, b: int):
  """
  Finds the greatest common divisor of a and b where b is greater than a.
  """
  if b == 0:
    return a
  return gcd(b, a % b)