# CSE 130: Programming Assignment 5
# crack.py
# Jay Ceballos
# A09338030
# jjceball@ieng6.ucsd.edu 

from misc import *
import crypt

def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""
    
    #>>> from crack import *
    #>>> load_words("words",r"^[A-Z].{2}$")
    #['A-1', 'AAA', 'AAE', ...]
    #>>> load_words("words",r"^xYx.*$")
    #[]

    final = []
    file1 = open(filename)
    pattern = re.compile(regexp)
    for word in file1.readlines():
        word = word.strip()
        if pattern.match(word):
            final.append(word)
    return final

def transform_reverse(str):
    """Return a list with the original string and the reversal of the original 
       string."""
    
    #>>> transform_reverse("Moose")
    #['Moose','esooM']

    reversed_l = [str]
    rString = str[::-1]
    reversed_l[len(reversed_l):] = [rString]
    return reversed_l

def transform_capitalize(str):
    """Return a list of all the possible ways to capitalize the input string.
       """
    
    #>>> transform_capitalize("foo")
    #['foo', 'Foo', 'fOo', 'FOo', 'foO', 'FoO', 'fOO', 'FOO']

    placeh = str.lower()
    words = [placeh]
    j = 0
    for element in words:
      for character in element:
        j = 0
        while (j < len(element)):
          s = element
          s = s[:j] + s[j].upper() + s[j+1:]
          if (s not in words): words.append(s)
          j += 1
    return words

def transform_digits(str):
    """Return a list of all possible ways to replace letters with similar 
       looking digits according to the following mappings. This should be done
       without regard to the capitalization of the input string, however when a
       character is not replaced with a digit, it's capitalization should be 
       preserved."""
    
    #>>> transform_digits("Bow")
    #['Bow', 'B0w', '6ow', '60w', '8ow', '80w']

    placeh = str
    words = [placeh]
    j = 0
    for element in words:
      for character in element:
        while(j < len(element)):
          s = element
          if (s[j] == 'o' or s[j] == 'O'):
            s = s[:j] + '0' + s[j+1:]
          elif (s[j] == 'z' or s[j] == 'Z'):
            s = s[:j] + '2' + s[j+1:]
          elif (s[j] == 'a' or s[j] == 'A'):
            s = s[:j] + '4' + s[j+1:]
          elif (s[j] == 'b'):
            s = s[:j] + '6' + s[j+1:]
          elif (s[j] == 'B'):
            s = s[:j] + '8' + s[j+1:]
            if (s not in words): words.append(s)
            s = s[:j] + '6' + s[j+1:]
          elif (s[j] == 'i' or s[j] == 'I'):
            s = s[:j] + '1' + s[j+1:]
          elif (s[j] == 'l' or s[j] == 'L'):
            s = s[:j] + '1' + s[j+1:]
          elif (s[j] == 'e' or s[j] == 'E'):
            s = s[:j] + '3' + s[j+1:]
          elif (s[j] == 's' or s[j] == 'S'):
            s = s[:j] + '5' + s[j+1:]
          elif (s[j] == 't' or s[j] == 'T'):
            s = s[:j] + '7' + s[j+1:]
          elif (s[j] == 'g' or s[j] == 'G'):
            s = s[:j] + '9' + s[j+1:]
          elif (s[j] == 'q' or s[j] == 'Q'):
            s = s[:j] + '9' + s[j+1:]
          else: pass
          if (s not in words): words.append(s)
          j = j + 1
        j = 0
    return words

def check_pass(plain,enc):
    """Check to see if the plaintext plain encrypts to the encrypted
       text enc"""
    
    #>>> check_pass("asarta","IqAFDoIjL2cDs")
    #True
    #>>> check_pass("foo","AAbcdbcdzyxzy")
    #False

    return crypt.crypt(plain,enc[:2]) == enc

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    
    #>>> load_passwd("passwd")
    #[{'account': 'root', 'shell': '/bin/bash', 'UID': 0, 'GID': 0, 'GECOS': 
    #'Corema Latterll', 'directory': '/home/root', 'password': 'VgzdTLne0kfs6'}, 
    #... ]

    openFile = open(filename,'r');
    passwords = []
    for s in openFile:
      list1 = ['account','password','UID','GID','GECOS','directory','shell']
      list2 = re.split('[:]',s)
      dictionary = make_dict(list1,list2)
      passwords.append(dictionary)
    return passwords

def crack_pass_file(fn_pass,words,out):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
    
    #>>> crack_pass_file("passwd","words","out")
    #Several minutes pass...
    #Ctrl-C
    #Traceback (most recent call last):
    #...
    #KeyboardInterrupt

    passwords = load_passwd(fn_pass)
    output = open(out,'w')
    cracked = []
    for passw in passwords:
      if ((passw['account'] != '\n') and (passw['password'] != None)):
        account = passw['account']
        pw = passw['password']
        successful_crack = ''
        success_or_fail = False
        possibles = open(words,'r')
        for poss in possibles:
          if(success_or_fail == True): break
          poss = poss.rstrip()
          poss = poss.lower()
          if (len(poss) > 8 or len(poss) < 6): pass
          else:        
            reverseList = transform_reverse(poss)
            for reverse in reverseList:
              success_or_fail = check_pass(reverse,pw)
              if(success_or_fail == True):
                successful_crack = reverse
                cracked.append(account)
                output.write(account + '=' + successful_crack + '\n')
                output.flush()
                break
              else: pass
    passwords = load_passwd(fn_pass)
    for passw in passwords:
      if ((passw['account'] != '\n') and (passw['password'] != None)):
        if (passw['account'] in cracked): pass
        else:
          account = passw['account']
          pw = passw['password']
          successful_crack = ''
          success_or_fail = False
          possibles = open(words,'r')
          j = 479625
          for poss in possibles:
            print(j)
            j = j - 1
            if(success_or_fail == True): break
            poss = poss.rstrip()
            poss = poss.lower()
            if (len(poss) > 8 or len(poss) < 6): pass
            else:
              reverseList = transform_reverse(poss)
              for reverse in reverseList:
                if (success_or_fail == True): break
                digitList = transform_digits(reverse)
                for digit in digitList:
                  success_or_fail = check_pass(digit,pw)
                  if (success_or_fail == True):
                    successful_cracked = digit
                    cracked.append(account)
                    print(account + '=' + successful_crack + '\n')
                    output.write(account + '=' + successful_crack + '\n')
                    output.flush()
                    break
                  else: pass