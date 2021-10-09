users = {"nisse":"apa", "bosse":"ko", "stina":"t-rex"}

print('Användare:')
for user in users:
    print(user)

print('Användare och Lösenord:')
for user in users:
    print(f"{user}) {users[user]}")
    
data = {"nisse":["luva", "vante"], "bosse":["spik", "skruv", "hammare"], "stina":["tidsmaskin"]}

print('Användare och deras data:')
for key in data:
    print(f"{key}) {data[key]}")

u = input('Slå upp användare: ')
if u in data:
    print(f"Data lagrat för {u}: {data[u]}")
else:
    print(f"Användare {u} finns inte")
