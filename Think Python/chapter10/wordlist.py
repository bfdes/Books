# Ex 10.9
def as_list(path: str):
  lines = []
  fin = open(path)
  for line in fin:
    word = line.strip()
    lines.append(word)
    # lines += [word] is an order of magnitude more expensive
  return lines
