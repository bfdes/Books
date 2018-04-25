# Ex 6.2
def ack(m: int, n: int):
  """
  Ackermann function. m, n >= 0.
  """
  if m == 0:
    return n + 1
  if(m > 0 and n == 0):
    return ack(m-1, 1)
  return ack(m-1, ack(m, n-1))
