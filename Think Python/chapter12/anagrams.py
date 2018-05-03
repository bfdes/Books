# Ex 12.2
def anagrams(words):
  d = {}
  for word in words:
    length, anagrams = d.setdefault(sorted(word), (len(word), []))
    anagrams.append(word)
  return d
