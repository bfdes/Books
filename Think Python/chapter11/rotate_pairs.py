from chapter8.cypher import rotate_word

# Ex 11.5
def rotate_pairs(words):
  pairs = []
  for word in words:
    for degree in range(1, 26 // 2):
      rotated = rotate_word(word, degree)
      if rotated in words:
        pairs.append((word, rotated))
  return pairs
