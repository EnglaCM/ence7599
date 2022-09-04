def flippblipp(n):
    if n%3==0 and n%5==0:
        print('flipp blipp')
    elif n%3==0:
        print('flipp')
    elif n%5==0:
        print('blipp')
    else:
        print(n)

n = 15

for i in range(1,n+1):
    flippblipp(i)