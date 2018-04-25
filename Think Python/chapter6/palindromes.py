# Ex 6.3
def first(word: str):
  return word[0]

def last(word: str):
  return word[-1]

def middle(word: str):
  return word[1:-1]

def is_palindrome(word: str):
  """
  Returns True if the word is a palindrome, False otheriwse.
  The empty string is considered a palindrome.
  """
  if len(word) == 0:
    return True
  if first(word) == last(word):
    return is_palindrome(middle(word))
  return False
