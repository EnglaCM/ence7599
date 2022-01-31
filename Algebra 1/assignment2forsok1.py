tal1 = 256217472
tal2 = 597840768
tal3 = 2017712592

""" Definition av delare: om num är en delare till tal, kan tal skrivas som num*b
där b är ett heltal.
Om num är ett heltal betyder det att även b är en delare till tal, ty num*b = tal.

När delare räknas gäller det alltså att för varje num som är delare till tal
är även (tal/num) en delare - vi kan alltså räkna +2 för varje num som är delare
till tal. När num > (tal/num) räknas dock num och (tal/num) som delare igen
Likt:
delare(24)
num     tal/num
1        24
2        12
3        8
4        6
6        4 -- num > tal/num
8        3
12       2
24       1
Därför fås det korrekta antalet delare genom att bara räkna på detta sätt fram
till att num < (tal/num).
Om däremot num*num = tal, finns en till delare, nämligen num. Eftersom
num = tal/num i detta scenario, räknas bara antaldelare +1 för detta num.
"""

def delare2 (tal):
    antalDelare = 0
    num = 1
    while num < (tal/num):
        if tal%num == 0:
            print(num, (tal//num))
            antalDelare += 2 #räknar både num och tal//num
        num += 1
    if num == (tal/num):
        antalDelare += 1 #räknar num om tal == num*num
        print(num)
    return antalDelare

#delare2(tal1) == 384
#delare2(tal2) == 384
#delare2(tal3) == 480

# checkfunktion - mycket långsam
def delare1 (tal):
    antalDelare = 0
    num = 1
    while num <= tal:#(tal//2):
        if tal%num == 0:
            antalDelare += 1
            print(num)
        num += 1
    #antalDelare += 1 #adderar 1 för att tal är en delare till sig själv
    return antalDelare

# delare1(tal1) == 384
# delare1(tal2) == 384
# delare1(tal3) == 480