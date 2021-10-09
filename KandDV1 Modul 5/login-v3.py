def menu(title,prompt,options):
    print(title)
    
    for i in options:
        print(f"{i}) {options[i]}")
    
    o = input(prompt)
    while o not in options:
        o = input(prompt)
        
    return o


def login(users):
    u = input('User: ')
    p = input('Password: ')

    while u not in users or users[u] != p:
        print()
        title = 'Invalid username or password'
        options = {'r': 'Try again', 'q': 'Quit'}
        prompt = 'Option: '
        
        a = menu(title,prompt,options)
        if a == 'q':
            return None
        else:
            print()
            u = input('User: ')
            p = input('Password: ')

    return u

users = {"nisse":"apa", "stina":"t-rex", "bosse":"ko"}

user = login(users)