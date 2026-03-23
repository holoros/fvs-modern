import sqlite3
import filecmp
import sys

"""
File Comparison Test
  Accepts two file names from the command line and compares the files

  Returns:
    If files contents are identical, "Comparion Test Passed" is printed to console.
    Else, "Comparison Test Failed" is printed to console.  
"""

if(filecmp.cmp(sys.argv[1], sys.argv[2])):
    print("Comparison Test Passed")
else:
    print("Comparison Test Failed")
