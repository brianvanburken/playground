// Topic: Result
//
// Requirements:
// * Determine if a customer is able to make a restricted purchase
// * Restricted purchases require that the age of the customer
//   is at least 21
//
// Notes:
// * Use a struct to store at least the age of a customer
// * Use a function to determine if a customer can make a restricted purchase
// * Return a result from the function
// * The Err variant should detail the reason why they cannot make a purchase

struct Customer {
    age: i8,
}

fn try_purchase_restricted(c: &Customer) -> Result<(), String> {
    if c.age >= 21 {
        Ok(())
    } else {
        Err("Customer is below age of 21".to_owned())
    }
}

fn main() {
    let bob = Customer { age: 20 };
    println!("{:?}", try_purchase_restricted(&bob))
}
