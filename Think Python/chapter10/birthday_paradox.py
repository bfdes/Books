from random import randint
from utils import has_duplicates

def generate_samples(number: int):
  samples = []
  for _ in range(number):
    sample = []
    for i in range(23):
      birthday = randint(1, 365)
      sample.append(birthday)
    samples.append(sample)
  return samples

# Ex 10.8
def probability(number: int):
  """
  Estimates the probability of any two members of a party of 23 people sharing a birthday.
  number: sample size
  """
  samples = generate_samples(number)
  count = 0
  for sample in samples:
    if has_duplicates(sample):
      count += 1
  return count / number
