# Ex 3.1
def right_justify(s):
  spaces = 70 - len(s)
  out = s
  for _ in range(0, spaces):
    out = ' ' + out
  print(out)
