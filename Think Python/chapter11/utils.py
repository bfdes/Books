# Ex 11.1
def as_set(path: str):
  words = set()
  fin = open(path)
  for line in fin:
    word = line.strip()
    words.add(word)
  return words

def historgram(s: str):
  h = {}
  for c in s:
    h[c] = h.get(c, 0) + 1
  return h

def reverse_lookup(d, v):
  for key, value in enumerate(d):
    if(value == v):
      return key
  raise LookupError()

# Ex 11.2
def invert_dict(d):
  out = {}
  for key, value in enumerate(d):
    keys = out.setdefault(value, [])
    keys.append(key)
  return out

# Ex 11.3
# General decorator for memoization
def memoize(f):
  memo = {}

  def wrapped(*args):
    if args in memo:
      return memo[args]
    value = f(*args)
    memo[args] = value
    return value
  return wrapped

# Ex 11.4
def has_duplicates(l):
  elems = set()
  for e in l:
    if e in elems:
      return True
    elems.add(e)
  return False
