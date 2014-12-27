# CSE 130: Programming Assignment 5
# misc.py
# Jay Ceballos
# A09338030
# jjceball@ieng6.ucsd.edu 
 
import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    
    #>>> from misc import * # this will load everything in misc.py
    #>>> closest_to([2,4,8,9],7)
    #8
    #>>> closest_to([2,4,8,9],5)
    #4

    if (l == None): 
      return None
    else:
      return min((abs(v - j), j) for j in l)[1]


def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    
    #>>> make_dict(["foo","baz"],["bar","blah"])
    #{'foo': 'bar', 'baz': 'blah'}
    #>>> make_dict([1],[100])
    #{1: 100}

    diction = {}
    for key, val in zip(keys,values):
      diction[key] = val
    return diction
   
# file IO functions
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
    
    #>>> word_count("news.txt")
    #{'all': 2, 'code': 2, 'gupta': 1, 'results': 1, 'four': 1, 'edu': 2, ...}

    word_count = {}
    openFile = open(fn,'r')
    delimit = re.compile('[^A-Za-z0-9_]')
    for line in openFile:
      words = delimit.split(line.lower())
      for w in words:
        if w in word_count: 
          word_count[w] = word_count[w] + 1
        else: 
          word_count[w] = 1
    del word_count['']
    return word_count