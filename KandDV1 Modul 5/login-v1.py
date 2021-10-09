users = {"nisse":"apa", "stina":"t-rex", "bosse":"ko"}

u = input('User: ')
p = input('Password: ')

while u not in users or users[u] != p:
    print('Invalid username or password')
    u = input('User: ')
    p = input('Password: ')

print(f"Welcome {u}")

