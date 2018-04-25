# Ex 5.4
def recurse(n, s):
  """
  Returns n/2(n+1+s) if n > -1, stackoverflow otherwise.
  """
  if n == 0:
    print(s)
  else:
    recurse(n-1, n+s)

if __name__ == '__main__':
  recurse(3, 0)  # 6
  # recurse(-1, 0) Stackoverflow since Python does not perform tail-call optimisation
