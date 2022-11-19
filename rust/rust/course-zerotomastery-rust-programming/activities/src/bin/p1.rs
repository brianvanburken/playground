// Project 1: Interactive bill manager
//
// Summary:
//   Create a command line bills/expenses manager that runs
//   interactively. This mini project brings together many of
//   the concepts learn thus far into a single application.
//
//   The user stories/requirements are split into stages.
//   Fully implement each stage as a complete working program
//   before making changes for the next stage. Leverage the
//   compiler by using `cargo check --bin p1` when changing
//   between stages to help identify adjustments that need
//   to be made.
//
// User stories:
// * Stage 1:
//   - I want to add bills, including the name and amount owed.
//   - I want to view existing bills.
// * Stage 2:
//   - I want to remove bills.
// * Stage 3:
//   - I want to edit existing bills.
//   - I want to go back if I change my mind.
//
// Tips:
// * Use the loop keyword to create an interactive menu.
// * Each menu choice should be it's own function, so you can work on the
//   the functionality for that menu in isolation.
// * A vector is the easiest way to store the bills at stage 1, but a
//   hashmap will be easier to work with at stages 2 and 3.
use std::io;

#[derive(Debug, Clone)]
pub struct Bill {
    name: String,
    amount: f64,
}

impl Bill {
    fn new(name: String, amount: f64) -> Self {
        Self { name, amount }
    }
}

#[derive(Debug)]
pub struct Bills {
    ledger: Vec<Bill>,
}

impl Bills {
    fn new() -> Self {
        Self { ledger: vec![] }
    }

    fn add_bill(&mut self, bill: Bill) {
        self.ledger.push(bill);
    }
}

fn get_input_string() -> Option<String> {
    let mut buffer = String::new();
    while io::stdin().read_line(&mut buffer).is_err() {
        println!("Please enter data again:")
    }
    let input = buffer.trim().to_owned();
    if input == "" {
        None
    } else {
        Some(input)
    }
}

fn get_input_number() -> Option<f64> {
    loop {
        let input = match get_input_string() {
            Some(input) => input,
            None => return None,
        };
        let parsed_input: Result<f64, _> = input.parse();
        match parsed_input {
            Ok(amount) => return Some(amount),
            Err(_) => println!("Please enter a correct number:"),
        }
    }
}

enum ManagerCommand {
    AddBill,
    ViewBills,
}

impl ManagerCommand {
    fn from_str(input: &str) -> Option<Self> {
        match input {
            "1" => Some(Self::AddBill),
            "2" => Some(Self::ViewBills),
            _ => None,
        }
    }

    fn print() {
        println!("1. Add bill");
        println!("2. View bills");
    }
}

mod actions {
    use crate::{get_input_number, get_input_string, Bill, Bills};

    pub fn add_bill(bills: &mut Bills) {
        println!("Input name of bill:");
        let name = match get_input_string() {
            Some(input) => input,
            None => return,
        };
        println!("Amount:");
        let amount = match get_input_number() {
            Some(amount) => amount,
            None => return,
        };
        let bill = Bill::new(name, amount);
        bills.add_bill(bill);
    }

    pub fn view_bills(bills: &Bills) {
        println!("{:?}", bills);
    }
}

fn main() {
    let mut bills = Bills::new();
    loop {
        println!("");
        println!("~~~ Bills Manager Actions ~~~");
        ManagerCommand::print();
        println!("");
        println!("Enter selection: ");
        let input = get_input_string().expect("No user input");
        match ManagerCommand::from_str(input.as_str()) {
            Some(ManagerCommand::AddBill) => actions::add_bill(&mut bills),
            Some(ManagerCommand::ViewBills) => actions::view_bills(&bills),
            None => return,
        }
    }
}
