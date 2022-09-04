def flippblipp(n):
    if n%3==0 and n%5==0:
        return 'flipp blipp'
    elif n%3==0:
        return 'flipp'
    elif n%5==0:
        return 'blipp'
    else:
        return str(n)

n = '1'
p = 1
print('      ',n)

while n == flippblipp(p):
    p += 1
    n = input('Nästa: ')
    
s = flippblipp(p)
print(f"Fel - {s}")
print()
print('Game Over')
print()