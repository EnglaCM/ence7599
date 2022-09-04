from menu import menu
#from translateVocab import translateVocab
#from loginwopassword import login


# Vill få in användarens vocab
# Frågar hur många för denna session

# vocab hämtas från translation-dictionaryn, tas bort därifrån och läggs till i en dicitonary -- blev list som innehåller tuples -- som håller de glosor som klassas som Hard
# Sen ska det även finnas medium och Easy dictionaries -- blev lists som innehåller tuples -- dit vocab flyttas till

# user och name är kopplat till login - kolla options
#users = ['engla']
#name = 'engla'
#user = login(users, name)
#user[name]['Hard'] = []
#user[name]['Easy'] = []
#user[name]['Medium'] = []
#user[name]['vocab'] = []
# Ovanstående kan beöva flyttas upp i filer

def glosovning(user, name):
    num = int(input('How many vocabs do you want to review? '))

    ### While hard och medium inte är tomma:
    #translateVocab(user,name)
    result = user[name]['vocab']
    #print(result)
    if num > len(user[name]['vocab']):
        num = len(user[name]['vocab'])

    for i in range(num):
        v = user[name]['vocab'].pop(0)
        user[name]['Hard'].append(v)

    while len(user[name]['Hard']) != 0 or len(user[name]['Medium']) != 0:
        for i in range(len(user[name]['Hard'])):
            word, trans = user[name]['Hard'].pop(0)       
            title = word
            options = {'a':'Show answer'}
            prompt = 'Option: ' 
            print('')
            a = menu(title, prompt, options)
            if a == 'Show answer':
                title = trans
                options = {'e':'Easy', 'm':'Medium', 'h':'Hard'}
                prompt = 'Option: ' 
                print('')
                b = menu(title, prompt, options)
                user[name][b].append((word,trans))
        
        for i in range(len(user[name]['Medium'])):
            word, trans = user[name]['Medium'].pop(0)       
            title = word
            options = {'a':'Show answer'}
            prompt = 'Option: ' 
            print('')
            a = menu(title, prompt, options)
            if a == 'Show answer':
                title = trans
                options = {'e':'Easy', 'm':'Medium', 'h':'Hard'}
                prompt = 'Option: '
                print('') 
                b = menu(title, prompt, options)
                user[name][b].append((word,trans))

    



#print(glosovning(user, name))