def view(description, strings):
    print(description)
    for n in range(len(strings)):
        print(f"{n+1}) {strings[n]}")
    return None

def add(prompt, strings):
    b = input(f"{prompt}: ")
    strings.append(b)
    return strings

strings = []

n = 5

print(n)

for i in range(n):
    add('Lägg till en sträng', strings)
    
s = f"Du har matat in följande {n} strängar"

view(s, strings)