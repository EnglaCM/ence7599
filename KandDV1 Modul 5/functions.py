def double(n):
    return 2*n

def triple(n):
    return 3*n

def quadruple(n):
    return 2*double(n)

def funky(n,m):
    return triple(n)+quadruple(m)

a = 3
b = 14

d1 = double(a)
d2 = double(b)

t1 = triple(a)
t2 = triple(b)

q1 = quadruple(a)
q2 = quadruple(b)

f1 = funky(a,b)
f2 = funky(b,b)