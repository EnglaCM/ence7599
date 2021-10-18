#from vetej import materialChoice
#from loginwopassword import login
#from options import chooseMaterial
from translateVocab import translateVocab
from glosovning import glosovning
from menu import menu


#users = ['engla'] # Tror även dessa ska flyttas upp i filer
#name = input('Username: ')
#user = login(users, name)

#user[name]['Hard'] = []
#user[name]['Easy'] = []
#user[name]['Medium'] = []
#user[name]['vocab'] = []
logout = False

def login(name):
    u = {}
    u[name] = {}
    global users
    
    if u[name] not in users:
        users.append(name)
        u[name]['new'] = True
        u[name]['Hard'] = []
        u[name]['Easy'] = []
        u[name]['Medium'] = []
        u[name]['vocab'] = []
    else:
        u[name]['new'] = False
    return u

def chooseMaterial(user, name):
    print()
    if user[name]['new'] == True:
        title = 'Welcome!'
        options = {'1':'Add new material', '2':'Log out'}
        prompt = 'Option: '
        user[name]['new'] = False
    else:
        title = 'Hello!'
        options = {'1':'Add new material', '2':'Use old material', '3':'Log out'}
        prompt = 'Option: '

    a = menu(title, prompt, options)
    return a

def materialChoice(user,name):
    a = chooseMaterial(user,name)

    if a == 'Add new material':
        translateVocab(user,name)
        glosovning(user, name)
    elif a == 'Use old material':
        glosovning(user, name)
    else:
        print ('See you soon!')
        global logout 
        logout = True


users = [] 
name = input('Username: ')
user = login(name)

while logout == False:
    materialChoice(user,name) 


# Nu har egentligen iinte login en funktion, men finns till för att man ska kunna lagra information om användarens vokabulär när appen utvecklas