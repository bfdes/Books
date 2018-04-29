# Ex 10.11
from wordlist import as_list
from bisection import in_bisect

if __name__ == '__main__':
  count = 0
  words = as_list('words.txt')
  for word in words:
    if in_bisect(words, word[::-1]):
      count += 1
  print(count, ' reverse pairs in the wordlist')
