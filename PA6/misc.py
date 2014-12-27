# CSE 130: Programming Assignment 6
# misc.py
# Jay Ceballos
# A09338030
# jjceball@ieng6.ucsd.edu 

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)