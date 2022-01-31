import math

b0 = 1
b1 = 1
n = 1 # n is the index of the last element in b
b = [b0,b1]
c = (3 + math.sqrt(13))/2

def kvotBC():
    global b
    global n
    global c
    k = abs((b[n]/b[n-1])-c)
    return k

def nextB():
    global b
    global n
    n += 1
    bn = 3*b[n-1] + b[n-2]
    b.append(bn)

def findBN():
    global b
    global n
    k = kvotBC()
    
    while k >= 10**(-12):
        nextB()
        k = kvotBC()
        
    return b[n]
    

print('Svar: b[n] = ', findBN())