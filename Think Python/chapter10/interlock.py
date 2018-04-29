# Ex 10.12
from wordlist import as_list
from bisection import in_bisect

def split(word):
  left = ''
  right = ''
  for i, char in enumerate(word):
    if (i % 2 == 0):
      left += char
    else:
      right += char
  return (left, right)

if __name__ == '__main__':
  words = as_list('words.txt')
  count = 0
  for word in words:
    left, right = split(word)
    if in_bisect(words, left) and in_bisect(words, right):
      count += 1
  print(count, 'interlocking pairs in the wordslist')
  # n.b. Cost of finding the count is n*ln n. Enumerating the pairs would cost n^2*lg n.
