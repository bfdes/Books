# Ex 10.1
def nested_sum(l):
  """
  Returns the sum of elements in a nested list.
  l: list of lists of numbers (one level of nesting)
  """
  total = 0
  for sublist in l:
    total += sum(sublist)
  return total

# Ex 10.2
def cumsum(l):
  out = []
  total = 0
  for e in l:
    total += e
    out.append(total)
  return out

# Ex 10.3
def middle(l):
  """
  Returns a new list containing all but the first and last elements of l.
  Raises AssertionError if l is not long enough.
  """
  if(len(l) < 2):
    raise AssertionError("List must have at least two elements")
  return l[1:len(l)-1]

# Ex 10.4
def chop(l):
  """
  Removes the first and last elements of l.
  Raises AssertionError if l is not long enough.
  """
  if(len(l) < 2):
    raise AssertionError("List must have at least two elements")
  l.pop(0)
  l.pop(len(l)-1)
  return l

# Ex 10.5
def is_sorted(l):
  length = len(l)
  if(length == 0 or length == 1):
    return True
  return l[0] <= l[1] and is_sorted(l[1:])

# Ex 10.7
def has_duplicates(l):
  sorted_list = sorted(l)
  for i, e in enumerate(sorted_list):
    if i == 0:
      continue
    if e == sorted_list[i-1]:
      return True
  return False
