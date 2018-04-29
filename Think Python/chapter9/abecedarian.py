# Ex 9.3
# n.b. Generalised version of has_no_e
def avoids(word: str, tokens: str):
  for char in word:
    if char in tokens:
      return False
  return True

# Ex 9.4
def uses_only(word: str, tokens: str):
  for char in word:
    if char not in tokens:
      return False
  return True

# Ex 9.5
def uses_all(word: str, tokens: str):
  for char in tokens:
    if char not in word:
      return False
  return True

# Ex 9.6
def is_abecedarian(word: str):
  """
  Returns True if the letters of word are ordered alphabetically.
  word: lowercased string
  """
  prev = 'a'
  for letter in word:
    if letter < prev:
      return False
    prev = letter
  return True
