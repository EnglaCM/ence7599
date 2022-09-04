def add(prompt, strings):
    b = input(f"{prompt}: ")
    strings.append(b)
    return strings

# hur ska en referens läggas till i add?

composers = ["Mozart", "Bach"]
print(f"Composers: {composers}")
print()

add("Add a composer: ", composers)
print()

print(f"Composers: {composers}")
print()