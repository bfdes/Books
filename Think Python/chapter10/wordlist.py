# Ex 10.9
def as_list(path: str):
  words = []
  fin = open(path)
  for line in fin:
    word = line.strip()
    words.append(word)
    # words += [word] is an order of magnitude more expensive
  return words
