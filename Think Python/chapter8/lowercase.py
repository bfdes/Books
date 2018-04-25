# Ex 8.4
def any_lowercase1(s: str):
  """
  Returns True if the first letter of s is lowercase, False otherwise.
  """
  for c in s:
    if c.islower():
      return True
    else:
      return False

def any_lowercase2(s: str):
  """
  Returns 'True' always.
  """
  for c in s:
    if 'c'.islower():
      return 'True'
    else:
      return 'False'

def any_lowercase3(s: str):
  """
  Returns True if the last letter of s is lowercase.
  """
  for c in s:
    flag = c.islower()
  return flag

def any_lowercase4(s: str):
  """
  Returns True if any letter if s is lowercase, False otherwise. (Correct impl.)
  """
  flag = False
  for c in s:
    flag = flag or c.islower()
  return flag

def any_lowercase5(s: str):
  """
  Returns True if s is lowercase, False otherwise.
  """
  for c in s:
    if not c.islower():
      return False
  return True
