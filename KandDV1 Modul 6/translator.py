from googletrans import Translator, constants
from pprint import pprint


translation = Translator().translate("Hola Mundo")
#print(f"{translation.origin} ({translation.src}) --> {translation.text} ({translation.dest})")


translator = Translator()
print(translator.translate('안녕하세요.', dest='en', src='ko'))

translations = translator.translate(['The quick brown fox', 'jumps over', 'the lazy dog'], dest='ko', src='en')
for translation in translations:
    print(translation.origin, ' -> ', translation.text)


translations = translator.translate(['The quick brown fox', 'jumps over', 'the lazy dog'], dest='ko')
for translation in translations:
    print(translation)

translator = Translator()
results =translator.translate('हॅलो वर्ल्ड')
print(results.text)

print(translator.detect('이 문장은 한글로 쓰여졌습니다.'))