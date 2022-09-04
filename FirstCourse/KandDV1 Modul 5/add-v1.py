ducks = ["Huey", "Dewey", "Louie"]

print(f"List of ducks: {ducks}")

a = input("Add a duck: ")

ducks.append(a)

print(f"Updated list of ducks: {ducks}")

def add(prompt, strings):
    b = input(f"{prompt}: ")
    strings.append(b)
    return f"Updated list: {strings}"