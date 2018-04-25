# Ex 6.5
def is_power(a: int, b: int):
  """
  Returns True if a is a power of b, False otherwise.
  Here a and b are integers where a > 0 and b != 0.
  """
  if a == 1:
    return True
  return (a % b == 0) and is_power(a / b, b)
