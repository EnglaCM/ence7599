from random import randint

def splash():
    print()
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    print("              Biathlon")
    print()
    print("         a hit or miss game")
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    print()
    return None

def open():
    return 0

def closed():
    return 1

def is_open(target):
    return target == open()

def is_closed(target):
    return target == closed()

def new_targets():
    t = []
    for i in range(5):
        t.append(open())
    return t

def close_target(target, targets):
    targets[target] = closed()
    return targets

def hits(targets):
    count = 0
    for t in targets:
        if is_closed(t):
            count += 1
    return count

def target_to_string(target):
    if is_open(target):
        return "* "
    elif is_closed(target):
        return "0 "
    
def targets_to_string(targets):
    s = ""
    for t in targets:
        s += target_to_string(t)
    return s

def view_targets(targets):
    print()
    print("0 1 2 3 4 ")
    print()
    print(targets_to_string(targets))
    print()
    return None

def random_hit():
    return randint(0,1) == 1

def shoot(targets, target):
    if random_hit():
        if targets[target] == closed():
            return "Hit on closed target"
        elif targets[target] == open():
            close_target(target, targets)
            return "Hit on open target"
    else:
        return "Miss"