"""((P1∧P2)∨(P3∧P4)∨(P5∧P6))⇒((P7∨P8)∧(P9∨P10)) är sant om:
((P1∧P2)∨(P3∧P4)∨(P5∧P6)) och ((P7∨P8)∧(P9∨P10)) är sanna
eller om ((P1∧P2)∨(P3∧P4)∨(P5∧P6)) är falskt och ((P7∨P8)∧(P9∨P10)) är falskt eller sant
"""
p1 = ["s","f"]
p2 = ["s","f"]
p3 = ["s","f"]
p4 = ["s","f"]
p5 = ["s","f"]
p6 = ["s","f"]
p7 = ["s","f"]
p8 = ["s","f"]
p9 = ["s","f"]
p10 = ["s","f"]


def komb (a, b):
    """ ger möjliga kombinationer av sanningsvärden från två, angivna p
    """
    res = []
    for s in a:
        for f in b:
            res.append(f+s)
    return res

def santDisVL(a,b,c):
    """
    tar kombinationer från p1-6 i tvåpar och ser om någon av paren är ss (att båda p är sanna och därmed även disjunktionen i VL),
    i sådana fall räknas dessa
    """
    kombinations = []
    res = 0
    for s in a:
        for f in b:
            for p in c:
                kombinations.append((s,f,p))
    for k in kombinations:
        if "ss" in k:
            res += 1
    return res

def falskDisVL(a,b,c):
    """ motsats till ovanstående, räknar om ingen är ss (och därmed är disjunktionen i VL falsk)
    """
    kombinations = []
    res = 0
    for s in a:
        for f in b:
            for p in c:
                kombinations.append((s,f,p))
    for k in kombinations:
        if "ss" not in k:
            res += 1
    return res

def santDis2 (a):
    """ ger de disjunktioner som är sanna av två givna p """
    res = []
    for s in a:
        if "ff" not in s:
            res.append(s)
    return res

def santKonHL (a,b):
    """ tar de disjunktioner av p7-p10 som är sanna och ger antalet kombinationer av dessa.
    Konjunktionen i HL är sann då både disjunktionen mellan p7 och p8, och mellan p9 och p10 är sanna.
    """
    kombinations = []
    res = []
    count = 0
    for s in a:
        for f in b:
            kombinations.append((s,f))
    for k in kombinations:
        count +=1
    return count

def HLsant (p7,p8,p9,p10):
    return santKonHL(santDis2(komb(p7,p8)),santDis2(komb(p9,p10)))

def HLallaKomb (p7,p8,p9,p10):
    """ alla möjliga kombinationer av p7-p10 är 2 möjliga utfall (sant resp. falskt) upphöjt till 4 (4 st angivna p som kan vara antingen sant eller falskt)
    """
    return 2**4

def VLsant(p1, p2, p3, p4, p5, p6):
    return santDisVL(komb(p1,p2),komb(p3,p4),komb(p5,p6))

def VLfalskt (p1, p2, p3, p4, p5, p6):
    return falskDisVL(komb(p1,p2),komb(p3,p4),komb(p5,p6))

""" resultatet är alla kombinationer då både VL och HL är sant adderat med antalet kombinationer då VL är falskt
    vilket fås av:
"""
result = VLsant(p1, p2, p3, p4, p5, p6)*HLsant (p7,p8,p9,p10) + VLfalskt (p1, p2, p3, p4, p5, p6)*HLallaKomb (p7,p8,p9,p10) # result = 765
