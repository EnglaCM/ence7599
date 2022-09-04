def login(users):
    u = input('User: ')
    p = input('Password: ')

    while u not in users or users[u] != p:
        print('Invalid username or password')
        u = input('User: ')
        p = input('Password: ')

    return u

