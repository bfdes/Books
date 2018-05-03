# Ex 12.4
# Reducible words are those which form a valid words when letters are successively removed.
# Recursively, a word is reducible if any of its children are reducible.
def children(word: str):
  """
  Returns a list of all the strings that can be formed by removing one character from this one.
  """
  children = []
  for i, _ in enumerate(word):
    child = ''.join([c for j, c in enumerate(word) if j != i])
    children.append(child)
  return children

# n.b. cannot use memoize decorator because set argument words is mutable.
def is_reducible(word, words):
  """
  Returns True if the word is reducible, False otherwise.
  word: valid word to test
  words: set of valid words in the language, including '', 'a', 'I'
  """
  if word == '':
    return True
  filtered = [c for c in children(word) if c in words]
  return any([is_reducible(c, words) for c in filtered])

