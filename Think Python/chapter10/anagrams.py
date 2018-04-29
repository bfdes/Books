# Ex 10.6
def is_anagram(word1: str, word2: str):
  return sorted(list(word1)) == sorted(list(word2))
