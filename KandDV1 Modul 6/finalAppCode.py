#from vetej import materialChoice
#from loginwopassword import login
from options import chooseMaterial
from translateVocab import translateVocab
from glosovning import glosovning

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

def materialChoice(user,name):
    a = chooseMaterial(user,name)

    if a == 'Add new material':
        translateVocab(user,name)
        glosovning(user, name)
        # Gå till funktion som sköter glosövningen
    elif a == 'Use old material':
        glosovning(user, name)
        # Gå till funktion som sköter glosövningen
    else:
        print ('See you soon!')
        global logout 
        logout = True
        #breakpoint

#materialChoice(user,name) # Så här kör den en gång

users = [] # Tror även dessa ska flyttas upp i filer
name = input('Username: ')
user = login(name)

while logout == False:
    materialChoice(user,name) 

# Problem som återstår är:
#  en nu inte kan fortsätta på gammalt material 
# det jag vill ska hända är att en fortsätter som u[name]['new'] = False i nästa loop av materialChoice
