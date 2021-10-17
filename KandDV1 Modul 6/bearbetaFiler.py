from chooseMaterial import viewMaterialOptions


a = viewMaterialOptions()

fileNames = {'Lupin (fr)': '[S01.E01]Lupin-Capitolo 1.French.srt', 'Les Intouchables (fr)':'IntouchablesDVDRIP.srt'}
file = fileNames[a]

subs = open(file)
print(subs.readline())


print(subs.readline())
print(subs.readline())
line = subs.readline()
word = line.strip()
print(word)
print(word)

subs = open(file)
num = ['0','1','2','3','4','5','6','7','8','9']
num = '0123456789'
d = dict()

for line in subs:
 word = line.strip()
 if len(word)>1 and word[0] not in num and word[-1] not in num: 
     #print(word)
     if word not in d:
         d[word] = 1
     else:
         d[word] += 1


     


#for line in subs:
 #word = line.strip()
 #print(word)

"""
# Exercise 9.1

def moreThan20(f):
  for line in f:
    word = line.strip()
    if len(word) >= 20:
      print(word)     

print(moreThan20(open('words.txt')))


# Exercise 9.2

def has_no_e(word):
  for letter in word:
    if letter in 'Ee':  # I changed this from if letter == 'e' since the previous one only checks if there is a lowercase e
     return False
  return True

print(has_no_e('telephone'))
print(has_no_e('ring'))
print(has_no_e('flora'))
print(has_no_e('elephant'))

def wordsWithoutE(f):
  count = 0
  countE = 0
  for line in f:
    word = line.strip()
    if 'E' not in word and 'e' not in word:
      count += 1
    else:
      countE += 1

  numberOfWords = count + countE
  percentage = (count/numberOfWords)*100
  return percentage

print(wordsWithoutE(open('words.txt')))
"""