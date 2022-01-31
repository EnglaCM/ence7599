import math

print(math.log(10))

def solvelog(n):
    num = 1
    while (num * math.log(num)/math.log(2)) < n:
        num += 1
    return (num-1)

def solvelogday(n):
    num = 2750000000
    while (num * math.log(num)/math.log(2)) < n:
        num += 1
    return (num-1)

def solvefact(n):
    num = 1
    i = 1
    while num < n:
        i += 1
        num = num*i

    return (i-1)