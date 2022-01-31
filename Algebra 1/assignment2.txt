tal1 = 256217472
tal2 = 597840768
tal3 = 2017712592

""" Definition av delare: om num är en delare till tal, kan tal skrivas som num*b
där b är ett heltal.
Om num är ett heltal betyder det att även b är en delare till tal, ty num*b = tal.

När delare räknas gäller det alltså att för varje num som är delare till tal
är även (tal/num) en delare - vi kan alltså appenda num och (tal/num) för varje
num som är delare till tal. När num > (tal/num) räknas dock num och
(tal/num) som delare igen
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
num = tal/num i detta scenario, appendas bara num till delareList
"""

def delare (tal):
    """ ger alla delare till tal
    """
    delareList = []
    num = 1
    while num < (tal/num):
        if tal%num == 0:
            delareList.append(num)
            delareList.append(tal//num)
        num += 1
    if num == (tal/num):
        delareList.append(num) 
    return delareList

""" En delare till talen 1 till 3 ger en rest på 0 vid division.
Då tal1 är minst, kan en delare inte vara större än tal1, eftersom definitionen
av en delare a till ett tal b är att b = a*c, där c är ett heltal. Om
a > b <=> c = b/a, vilket inte är ett heltal. Därför kollas enbart om delarna till
tal1 är delare till tal2 och tal3, istället för att gå igenom alla möjliga
delare till dessa med
"""

def gemensammadelare (delareList, tal2, tal3):
    """delareList behöver vara listan av delare (från delare()) av det
    minsta talet (tal1). tal2, och tal3 är de tal vars gemensamma delare
    med tal1 ska finnas. När en delare till tal1 även ger 0 i rest vid
    division med tal2 och tal3, är det en delare till alla tal 1-3
    """
    antalDelare = 0
    for delare in delareList:
        if tal2%delare == 0 and tal3%delare == 0:
            antalDelare += 1
    return antalDelare

print(gemensammadelare(delare(tal1), tal2, tal3)) #Svar: 180



def delareTest (tal1, tal2, tal3):
    """Denna funktion är bara test. Den går mycket långsamt
    """
    antalDelare = 0
    num = 1
    while num <= tal1:
        if tal1%num == 0 and tal2%num == 0 and tal3%num == 0:
            antalDelare += 1
        num += 1
    return antalDelare

#delareTest(tal1,tal2,tal3) == 180