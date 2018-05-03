import string
import random
from bisect import bisect_right
from os import path
from operator import itemgetter
from chapter10.utils import cumulative_sum
from chapter11.utils import as_set

def histogram(words):
  """
  Returns a historgram of the words in the given list.
  """
  d = {}
  for word in words:
    d[word] = d.get(word, 0) + 1
  return d

# Ex 13.1
def process_file(path: str):
  """
  Process a text file given its path, stripping out whitespace and punctuation.
  Returns the file as a mapping of word-frequency pairs (histogram).

  n.b. Loads all the words of the file into memory.
  """
  fin = open(path)

  def process_line(line: str):
    words = []
    line = line.replace('-', ' ').strip() # Book regards hyphenated words as pairs
    for word in line.split(' '):
      word = word.strip(string.whitespace + string.punctuation).lower()
      words.append(word)
    return words
  
  words = []

  for line in fin:
    words.extend(process_line(line))
    
  return histogram(words)

# Ex 13.2
def distinct_words(h):
  return len(h)  # No. of ditinct words is the no. of keys in the histogram

def total_words(h):
  return sum(h.values())

# Ex 13.3
def most_frequent(h, n=10):
  """
  Returns n most frequent words in the corpus.
  """
  ordered =  sorted(h.items(), key=itemgetter(1), reverse=True)
  return [word for i, (word, _) in enumerate(ordered) if i < n]


# Ex 13.4, 13.6
def typos(h, wordslist):
  """
  Identify the typos in a corpus summarised as a histogram.
  h: histogram of corpus
  wordslist: valid words in vocabulary, as a set
  returns: set of typos
  """
  return set(h.keys()) - wordslist

# Ex 13.5, 13.7
def random_word(h):
  """
  Efficient method for uniformally selecting a word from the corpus.
  """
  distinct_words = list(h.keys())
  cumulative_sums = cumulative_sum(h.values())
  n = total_words(h)
  i = random.randint(0, n)
  j = bisect_right(cumulative_sums, i)
  return distinct_words[j]


if __name__ == '__main__':
  book = path.abspath(path.join(__file__, '../emma.txt'))
  words = path.abspath(path.join(__file__, '../words.txt'))
  # Assume the header is insignificant compared with the length of the text.
  # The headers take a different form for every ebook anyway
  h = process_file(book)
  vocabulary = as_set(words)
  print("There are ", total_words(h), " words in the book.")
  print("Only ", distinct_words(h), " are distinct.")
  print("There are ", str(len(typos(h, vocabulary))), " typos.")
  print("The ten most frequent words are ", str(most_frequent(h)), '.')
