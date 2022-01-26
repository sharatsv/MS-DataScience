#!/usr/bin/python

# Example-1
print 'Example-1:'
samples=['at', 'bat', 'c', 'dog', 'eel', 'frog', 'golden', 'hen', 'ink', 'jog']

# Using a list-comprehension
select_words = [word for word in samples if len(word) > 2]
print 'Words with more than two letters:', select_words

# Print the words individually
for word in select_words:
    print word

# Example-2
print 'Example-2:'
samples=['at', 'bat', 'c', 'dog', 'elephant', 'frog', 'golden', 'hen', 'ink', 'jog']

# Using a list-comprehension
select_words = [word for word in samples if len(word) > 2 and len(word) < 5]
print 'Where: 2 < words < 5:', select_words

# Print the words individually
for word in select_words:
    print word

'''
Output from the program:
$ python Sharat_Sripada_Lists_1.5.py
Example-1:
Words with more than two letters: ['bat', 'dog', 'eel', 'frog', 'golden', 'hen', 'ink', 'jog']
bat
dog
eel
frog
golden
hen
ink
jog
Example-2:
Where: 2 < words < 5: ['bat', 'dog', 'frog', 'hen', 'ink', 'jog']
bat
dog
frog
hen
ink
jog
'''
