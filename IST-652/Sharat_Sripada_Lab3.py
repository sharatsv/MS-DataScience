# Problem-1: 
def multiply(mylist):
    res = 1
    for i in mylist:
        res *= i
    print('Multiply elements in {0} = {1}'.format(mylist, res))

print('Problem-1:')
print('----------')
input_list = [8, 2, 3, -1, 7]
multiply(input_list)
input_list = [8, 2, 3, -2, 7]
multiply(input_list)
input_list = [8, 2, 3, -3, 7]
multiply(input_list)
input_list = [8, 2, 3, -4, 7]
multiply(input_list)

# Problem-2:
print('\nProblem-2:')
print('----------')
def upperlower(mystr):
    upper = 0
    lower = 0
    for i in mystr:
        if i.isupper():
            upper += 1
        elif i.islower():
            lower += 1
    print('Count of upper/lower case alphabets in sentence: {0}'.format(mystr))
    print('Upper={0} Lower={1}'.format(upper, lower))

upperlower('The quick Brown Fox')
upperlower('The ReAlLy quick Brown Fox')
upperlower('The rEaLlY ReAlLy quick Brown Fox')
