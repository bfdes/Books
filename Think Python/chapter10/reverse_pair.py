# Ex 10.11
from os import path
from wordlist import as_list
from bisection import in_bisect

if __name__ == '__main__':
  count = 0
  sample = path.abspath(path.join(__file__, '../words.txt'))
  words = as_list(sample)
  for word in words:
    if in_bisect(words, word[::-1]):
      count += 1
  print(count, ' reverse pairs in the wordlist')
