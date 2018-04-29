from bisect import bisect_left

# Ex 10.10
def in_bisect(l, word):
  i = bisect_left(l, word)
  return i != len(l) and l[i] == word
