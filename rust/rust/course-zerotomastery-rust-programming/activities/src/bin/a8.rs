// Topic: Organizing similar data using structs
//
// Requirements:
// * Print the flavor of a drink and it's fluid ounces
//
// Notes:
// * Use an enum to create different flavors of drinks
// * Use a struct to store drink flavor and fluid ounce information
// * Use a function to print out the drink flavor and ounces
// * Use a match expression to print the drink flavor

enum Flavour {
    Lemon,
    Strawberry,
}

struct Drink {
    flavour: Flavour,
    fluid_in_ounces: i32,
}

fn main() {
    display_drink(Drink {
        flavour: Flavour::Lemon,
        fluid_in_ounces: 10,
    })
}

fn display_drink(drink: Drink) {
    display_flavour(drink.flavour);
    println!("oz: {:?}", drink.fluid_in_ounces)
}

fn display_flavour(flavour: Flavour) {
    match flavour {
        Flavour::Lemon => println!("lemon"),
        Flavour::Strawberry => println!("strawberry"),
    }
}
