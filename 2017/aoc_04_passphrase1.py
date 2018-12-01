'''
--- Day 4: High-Entropy Passphrases ---

A new system policy has been put in place that requires all accounts to use a passphrase instead of simply a password. A passphrase consists of a series of words (lowercase letters) separated by spaces.

To ensure security, a valid passphrase must contain no duplicate words.

For example:

    aa bb cc dd ee is valid.
    aa bb cc dd aa is not valid - the word aa appears more than once.
    aa bb cc dd aaa is valid - aa and aaa count as different words.

The system's full passphrase list is available as your puzzle input. How many passphrases are valid?

[puzzle input: in a separate file, 04_imput.txt ]
'''
from collections import Counter

f = open('04_input.txt', 'r')

def check_validity(passphrase):
    pass_list = passphrase.split()
    pass_set = set(pass_list)
    return len(pass_set) == len(pass_list)

def check_validity_file(f):
    results = [check_validity(line) for line in f]
    return results

def count_valid_phrases(validated_list):
    c = Counter(validated_list)
    return c[True]

validated_list = check_validity_file(f)
truths = count_valid_phrases(validated_list)
print('The number of valid passphrases is %s.' % truths)
