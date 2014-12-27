# CSE 130: Programming Assignment 6
# vector.py
# Jay Ceballos
# A09338030
# jjceball@ieng6.ucsd.edu 

from misc import Failure

class Vector(object):
  def __init__(self, arg):
    """Constructor for the Vector class. The constructor should take a single 
    argument. If this argument is either an int or a long or an instance of a 
    class derived from one of these, then consider this argument to be the length 
    of the Vector. In this case, construct a Vector of the specified length with 
    each element is initialized to 0.0. If the length is negative, raise a 
    ValueError with an appropriate message. If the argument is not considered to 
    be the length, then if the argument is a sequence (such as a list), then 
    initialize with vector with the length and values of the given sequence. If t
    he argument is not used as the length of the vector and if it is not a 
    sequence, then raise a TypeError with an appropriate message."""
    try:
      if arg < 0:
        raise ValueError("length cannot be negative")
    except TypeError:
      pass
    try:
      self.data = [0.0] * arg
    except Exception:
      self.data = list(arg)
  
  def __repr__(self):
    """Method to return a string of python code which could be used to initialize 
    the Vector. This string of code should consist of the name of the class 
    followed by an open parenthesis followed by the contents of the vector 
    represented as a list followed by a close parenthisis."""
    return "Vector(" + repr(self.data) + ")"

  def __len__(self):
    """Return the length of the Vector."""
    return len(self.data)

  def __iter__(self):
    """Return an object that can iterate over the elements of the Vector."""
    for e in self.data:
      yield e
  
  def __add__(self, arg):
    """Add the vector as well as another vector"""
    return Vector(([x + y for x, y in zip(list(self), list(arg))]))

  def __radd__(self, arg):
    """Reflected (swapped) operands"""
    return Vector(([x + y for x, y in zip(list(self), list(arg))]))

  def __iadd__(self, arg):
    """Augmented Arithmetic Assignments"""
    self.data =  Vector(([x + y for x, y in zip(list(self), list(arg))]))
    return self.data

  def dot(self, arg):
    """Takes either a Vector or a sequence and returns the dot product of 
    the argument with current Vector instance. The dot product is defined as 
    the sum of the component-wise products. The behavior of this function if 
    any elements are not numeric is undefined."""
    try:
      return sum([x * y for x, y in zip(self, arg)])
    except:
      return sum([str(x) + str(y) for x, y in zip(self, arg)])

  def __getitem__(self, n):
    """Allows element level access to the Vector. Indexing should be 0 based 
    (as in C). If the index is negative, it should translate to the length of 
    the Vector plus the index. Thus, index -1 is the last element. If the index 
    is out of range, your implementation should raise an IndexError with an 
    appropriate message. This behavior should be identical to that of a list."""
    return self.data[n]

  def __setitem__(self, n, x):
    """Allows element level access to the Vector. Indexing should be 0 based 
    (as in C). If the index is negative, it should translate to the length of 
    the Vector plus the index. Thus, index -1 is the last element. If the index 
    is out of range, your implementation should raise an IndexError with an 
    appropriate message. This behavior should be identical to that of a list."""
    self.data[n] = x

  "____________________________________________________________________________"

  """Implement comparison functions for Vectors. Two vectors should be considered 
  equal if each element in the first Vector is equal to the respective element in 
  the second Vector. A Vector, a, should be considered greater than a Vector, b, 
  if the largest element of a is greater than the largest element of b. If the 
  largest elements of both are equal, then compare the second-largest elements, 
  and so forth. If every pair compared in this fashion is equal, then a should 
  not be considered greater than b, but a should be considered greater than or 
  equal to b. If a is greater than b, then b is less than a. This is a nonstandard 
  ethod for comparing vectors, and for a pair of vectors v and w, v>=w does not 
  imply that v>w or v==w. When a Vector is compared to something that isn't a 
  Vector, they should never be equal, and the result should be the same as if 
  the respective comparison function were called in in the class object."""

  def __gt__(self, arg):
    if not isinstance(arg, Vector):
      return self > arg
    for x, y in self.cmp_helper(arg):
      if not x > y:
        return False
    return True

  def __ge__(self, arg):
    if not isinstance(arg, Vector):
      return self >= arg
    for x, y in self.cmp_helper(arg):
      if not x >= y:
        return False
    return True

  def __lt__(self, arg):
    return not self.__ge__(arg)

  def cmp_helper(self, arg):
    """Return the vector in descending order"""
    return zip(sorted(self, reverse=True), sorted(arg, reverse=True))