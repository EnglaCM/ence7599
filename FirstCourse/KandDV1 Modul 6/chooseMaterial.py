from menu2 import menu


def viewMaterialOptions():
    title = 'Do you want to add your own material or use already existing material'
    options = {'1':'Add your own material', '2':'Use material from database'}
    prompt = 'Option: '

    a = menu(title, prompt, options)
    
    if a == '1':
        result = options[a]
    
    if a == '2':
        result = viewCategoryOptions()
    
    return result

def viewCategoryOptions():
    title = 'Choose your prefered catergory'
    options = {'1':'Lupin (fr)', '2':'Les Intouchables (fr)'}
    prompt = 'Option: '
        
    a = menu(title, prompt, options)
    
    return options[a]
