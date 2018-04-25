# Ex 5.1
from time import time

def formatted(timestamp):
  days, rem = divmod(timestamp, 24*60*60)
  hours, rem = divmod(rem, 60*60)
  mins, secs = divmod(rem, 60)
  return (days, hours, mins, secs)

if __name__ == '__main__':
  print(formatted(time()))