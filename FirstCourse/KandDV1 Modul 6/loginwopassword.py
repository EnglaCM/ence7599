users = ['engla']

def login(users, name):
    u = {}
    u[name] = {}
    
    if u[name] not in users:
        users.append(u[name])
        u[name]['new'] = True
        u[name]['Hard'] = []
        u[name]['Easy'] = []
        u[name]['Medium'] = []
        u[name]['vocab'] = []
    else:
        u[name]['new'] = False
    return u

#users = {} #user och users kommmer behöva vara i filen som hämtar denna function tror jag.

#user = login(users)
#print(user, users)