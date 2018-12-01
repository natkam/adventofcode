'''
--- Part Two ---

For added security, yet another system policy has been put in place. Now, a valid passphrase must contain no two words that are anagrams of each other - that is, a passphrase is invalid if any word's letters can be rearranged to form any other word in the passphrase.

For example:

    abcde fghij is a valid passphrase.
    abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
    a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
    iiii oiii ooii oooi oooo is valid.
    oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.

Under this new system policy, how many passphrases are valid?

(The input is in the same file, 04_input.txt)
'''
from collections import Counter

f = open('04_input.txt', 'r')

def check_validity(passphrase):
    parsed_list = [''.join(sorted(word)) for word in passphrase.split()]
    parsed_set = set(parsed_list)
    return len(parsed_set) == len(parsed_list)

def check_validity_file(f):
    results = [check_validity(line) for line in f]
    return results

def count_valid_phrases(validated_list):
    c = Counter(validated_list)
    return c[True]

validated_list = check_validity_file(f)
truths = count_valid_phrases(validated_list)
print('The number of valid passphrases is %s.' % truths)
