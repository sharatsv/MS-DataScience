#!/usr/bin/python

while True:
    try:
        score=input('Enter score (0-1.0):')
        if score >= 0.9 and score < 1.0:
            print 'Grade: A'
        elif score >= 0.8 and score < 0.9:
            print 'Grade: B'
        elif score >= 0.7 and score < 0.8:
            print 'Grade: C'
        elif score >= 0.6 and score < 0.7:
            print 'Grade: D'
        elif score < 0.6:
	    print 'Grade: F'
        else:
            print 'Bad Score'
    except:
	print 'Bad Score'

''' 
Output from the program:
$ python Sharat_Sripada_Boolean_1.4.py
Enter score (0-1.0):0.95
Grade: A
Enter score (0-1.0):perfect
Bad Score
Enter score (0-1.0):10.0
Bad Score
Enter score (0-1.0):0.75
Grade: C
Enter score (0-1.0):0.5
Grade: F
'''
