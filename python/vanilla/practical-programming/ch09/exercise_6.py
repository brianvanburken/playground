text = ''
while text.lower() != 'quit':
    text = input("Please enter a chemical formula (or 'quit' to exit): ")
    if text.lower() == "quit":
        print("...exiting program")
    elif text == "H2O":
        print("Water")
    elif text == "NH3":
        print("Ammonia")
    elif text == "CH4":
        print("Methane")
    else:
        print("Unkown compound")
