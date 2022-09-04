def menu(title,prompt,options):
    print(title)
    
    for i in options:
        print(f"{i}) {options[i]}")
    
    o = input(prompt)
    while o not in options:
        o = input(prompt)
        
    return o

k = {"a":"Add item", "l":"List items", "q":"Log out"}
o = menu('Meny','Välj: ', k)
print(f"Du valde {o}) {k[o]}") 

options1 = {"a":"Add item", "l":"List items", "q":"Log out"}
opt1 = menu("Select an action", "Action: ", options1)
print(f"You selected action {opt1}) {options1[opt1]}")