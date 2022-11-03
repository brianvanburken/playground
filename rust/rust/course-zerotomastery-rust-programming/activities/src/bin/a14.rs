// Topic: Strings
//
// Requirements:
// * Print out the name and favorite colors of people aged 10 and under
//
// Notes:
// * Use a struct for a persons age, name, and favorite color
// * The color and name should be stored as a String
// * Create and store at least 3 people in a vector
// * Iterate through the vector using a for..in loop
// * Use an if expression to determine which person's info should be printed
// * The name and colors should be printed using a function

struct Person {
    age: i8,
    name: String,
    favorite_color: String,
}

fn print(val: &String) {
    println!("{:?}", val)
}

fn main() {
    let people = vec![
        Person {
            age: 18,
            name: "Alice".to_owned(),
            favorite_color: "pink".to_owned(),
        },
        Person {
            age: 19,
            name: "Bob".to_owned(),
            favorite_color: "purple".to_owned(),
        },
        Person {
            age: 20,
            name: "Charlie".to_owned(),
            favorite_color: "blue".to_owned(),
        },
    ];

    for person in people {
        if person.name == "Bob" {
            print(&person.name);
            print(&person.favorite_color);
        }
    }
}
