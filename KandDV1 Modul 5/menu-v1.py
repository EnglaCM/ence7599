options = {"a":"Add item", "l":"List items", "q":"Log out"}

print ('Select an action')
for a in options:
    print(f"{a}) {options[a]}")

o = input('Option: ')

while o not in options:
    o = input('Option: ')

print(f"You selected option {o}) {options[o]}")

