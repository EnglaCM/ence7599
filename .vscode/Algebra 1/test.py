def f (x):
    return 4*x^3 + 12*x^2 + 6*x + 12

def findx():
    x = 0
    k = f(x)
    while x < 100:
        if f(x) == 0:
            return x
        x += 1
    return 'none'