import random
import string
from os import path

def process_file(path: str):
  fin = open(path)

  def process_line(line: str):
    words = []
    line = line.replace('-', ' ').strip()
    for word in line.split(' '):
      # n.b. Textbook does not strip punctuation or ignore case
      word = word.strip(string.whitespace)
      words.append(word)
    return words
  
  words = []
  for line in fin:
    words.extend(process_line(line))
    
  return words

# Ex 13.8
def analyse(words, prefix_length=2):
  n = prefix_length
  d = {}
  for i, _ in enumerate(words):
    if(i+n > len(words)-1):
      break
    prefix = tuple(words[i:i+n])
    suffix = words[i+n]
    suffixes = d.setdefault(prefix, set())
    suffixes.add(suffix)
  return d

# Ex 13.8
def walk(words, links=10, prefix_length=2):
  # Convert the suffixes to lists so we use random operations on them
  mapping = { prefix: list(suffixes) for prefix, suffixes in analyse(words, prefix_length).items()}
  prefixes = list(mapping.keys())

  # We must have at least one prefix and all prefixes must appear in prefix-suffix pairs
  assert len(prefixes) > 0
  assert all([len(suffixes) > 0 for suffixes in mapping.values()])

  chain = []
  n = 0
  link = None

  while(n < links):
    if(link is None):
      # Start a new fragment
      link = random.choice(prefixes)
      chain.extend(list(link))
      n += 1
      continue
    try:
      suffix = random.choice(mapping[link])
      chain.append(suffix)
      _, *rest = link
      link = *rest, suffix
      n += 1
    except KeyError:
      # Fragment broken because we can't find a suffix, so start a new one
      link = None    
  
  return ' '.join(chain).strip()
