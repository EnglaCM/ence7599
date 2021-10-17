from menu import menu

def chooseMaterial(user, name):
    print()
    if user[name]['new'] == True:
        title = 'Welcome!'
        options = {'1':'Add new material', '2':'Log out'}
        prompt = 'Option: '
        user[name]['new'] == False
    else:
        title = 'Hello!'
        options = {'1':'Add new material', '2':'Use old material', '3':'Log out'}
        prompt = 'Option: '

    a = menu(title, prompt, options)
    return a

