// Topic: Advanced match
//
// Requirements:
// * Print out a list of tickets and their information for an event
// * Tickets can be Backstage, Vip, and Standard
// * Backstage and Vip tickets include the ticket holder's name
// * All tickets include the price
//
// Notes:
// * Use an enum for the tickets with data associated with each variant
// * Create one of each ticket and place into a vector
// * Use a match expression while iterating the vector to print the ticket info

#[derive(Debug)]
enum Ticket {
    Backstage { name: String, price: f32 },
    Standard(f32),
    Vip { name: String, price: f32 },
}

fn main() {
    let tickets = vec![
        Ticket::Backstage {
            name: "Bob".to_owned(),
            price: 1000.0,
        },
        Ticket::Vip {
            name: "Alice".to_owned(),
            price: 0.0,
        },
        Ticket::Standard(120.50),
    ];

    for ticket in tickets {
        match ticket {
            Ticket::Backstage { name, price } => {
                println!("Backstage, name={:?}, price={:?}", name, price)
            }
            Ticket::Vip { name, price } => println!("Vip, name={:?}, price={:?}", name, price),
            Ticket::Standard(price) => println!("Standard, price={:?}", price),
        }
    }
}
