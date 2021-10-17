from googletrans import Translator, constants
from pprint import pprint

def translateVocab(user,name):
    d = dict()
    text = input('Add your text: ')
    vocab = []
    result = []

    words = text.split(' ')
    for word in words:
        word = word.lower()
        # ta bort specialtecken:
        word = ''.join(filter(str.isalnum, word))
        if word not in d:
            d[word] = 1
        else:
            d[word] += 1
    # sorterar orden utifrån fallande ordning med högst frekvens som value i d först i vocab
    for w in sorted(d, key=d.get, reverse=True):
        #print(w, d[w])
        vocab.append(w)


    translator = Translator()
    #print(translator.translate('안녕하세요.'))

    translations = translator.translate(vocab, dest='en')
    for translation in translations:
        result.append((translation.origin, translation.text))
        #print(translation.origin, translation.text)

    user[name]['vocab'] += result
    #print(result)