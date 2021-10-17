from options import chooseMaterial
from loginwopassword import login
from translateVocab import translateVocab
from glosovning import glosovning
#from menu import menu



#users = ['engla'] # Tror även dessa ska flyttas upp i filer
#name = input('Username: ')
#user = login(users, name)

#user[name]['Hard'] = []
#user[name]['Easy'] = []
#user[name]['Medium'] = []
#user[name]['vocab'] = []
#end = False

# får in antingen 'Add new material', 'Use old material' eller 'Log out' från options.py
def materialChoice(user,name):
    a = chooseMaterial(user,name)

    if a == 'Add new material':
        translateVocab(user,name)
        glosovning(user, name)
        # Gå till funktion som sköter glosövningen
    elif a == 'Use old material':
        print('hej')
        glosovning(user, name)
        # Gå till funktion som sköter glosövningen
    else:
        print()
        print ('See you soon!')
        global logout 
        logout = True
        



#while end != True:
#materialChoice(user, name)


#### FÅ TILL EN LOOP MED materialChoice och glosovning så att en kommer tillbaka till valet att lägga till fler om en vill. 
# Möjligtvis ska detta ske i finalAppCode där en även sparar listan a redan existerande användare