# Ex 12.3
# By defintion the members of a methathesis pair have to also be anagrams

def is_pair(word1, word2):
  """
  Determines whether the anagrams word1, word2 also form a metathesis pair.
  """
  z = zip(word1, word2)
  unique = [(x, y) for x, y in z if x != y]
  return len(unique) == 2

def metathesis_pairs(mapping):
  """
  Returns all the metathesis pairs from the anagram dictionary.
  mapping: dictionary mapping sorted anagram key to (len(key), anagrams).
  """
  pairs = []
  for _, anagrams in mapping.values():
    for a in anagrams:
      for b in anagrams:
        if a < b and is_pair(a, b):
          pairs.append((a, b))
  return pairs
