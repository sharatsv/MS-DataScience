#!/usr/bin/python

def computepay(hours, rate):
    print('Pay: {:.2f}'.format(int(hours) * int(rate)))

in_hours = input('Enter hours:')
in_rate = input('Enter rate:')

computepay(in_hours, in_rate)
