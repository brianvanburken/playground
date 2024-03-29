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
use std::{collections::HashMap, io};

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
    ledger: HashMap<String, Bill>,
}

impl Bills {
    fn new() -> Self {
        Self {
            ledger: HashMap::new(),
        }
    }

    fn add_bill(&mut self, bill: Bill) {
        self.ledger.insert(bill.name.to_string(), bill);
    }

    fn remove(&mut self, name: &String) -> bool {
        self.ledger.remove(name).is_some()
    }

    fn update(&mut self, name: &str, amount: f64) -> bool {
        if let Some(bill) = self.ledger.get_mut(name) {
            bill.amount = amount;
            return true;
        }
        false
    }

    fn print(&self) {
        println!("{:?}", self.ledger.values())
    }
}

fn get_input_as_string() -> Option<String> {
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

fn get_input_as_number() -> Option<f64> {
    loop {
        let input = match get_input_as_string() {
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
    RemoveBill,
    UpdateBill,
}

impl ManagerCommand {
    fn from_str(input: &str) -> Option<Self> {
        match input {
            "1" => Some(Self::AddBill),
            "2" => Some(Self::ViewBills),
            "3" => Some(Self::RemoveBill),
            "4" => Some(Self::UpdateBill),
            _ => None,
        }
    }

    fn print() {
        println!("1. Add bill");
        println!("2. View bills");
        println!("3. Remove bill");
        println!("4. Update bill");
    }
}

mod actions {
    use crate::{get_input_as_number, get_input_as_string, Bill, Bills};

    pub fn add_bill(bills: &mut Bills) {
        println!("Input name of bill:");
        let name = match get_input_as_string() {
            Some(input) => input,
            None => return,
        };
        println!("Amount:");
        let amount = match get_input_as_number() {
            Some(amount) => amount,
            None => return,
        };
        let bill = Bill::new(name, amount);
        bills.add_bill(bill);
    }

    pub fn remove_bill(bills: &mut Bills) {
        bills.print();
        println!("Enter bill name to remove:");
        let bill_name = match get_input_as_string() {
            Some(input) => input,
            None => return,
        };
        if bills.remove(&bill_name) {
            println!("Bill is removed.")
        } else {
            println!("Bill not found!")
        }
    }

    pub fn view_bills(bills: &Bills) {
        bills.print();
    }

    pub fn update_bill(bills: &mut Bills) {
        bills.print();
        println!("Enter bill name to update:");
        let bill_name = match get_input_as_string() {
            Some(input) => input,
            None => return,
        };
        println!("New amount for bill:");
        let amount = match get_input_as_number() {
            Some(amount) => amount,
            None => return,
        };
        if bills.update(&bill_name, amount) {
            println!("Bill is updated.")
        } else {
            println!("Bill not found!")
        }
    }
}

fn run_program() -> Option<()> {
    let mut bills = Bills::new();
    loop {
        println!("");
        println!("~~~ Bills Manager Actions ~~~");
        ManagerCommand::print();
        println!("");
        println!("Enter selection: ");
        let input = get_input_as_string()?;
        match ManagerCommand::from_str(input.as_str()) {
            Some(ManagerCommand::AddBill) => actions::add_bill(&mut bills),
            Some(ManagerCommand::ViewBills) => actions::view_bills(&bills),
            Some(ManagerCommand::RemoveBill) => actions::remove_bill(&mut bills),
            Some(ManagerCommand::UpdateBill) => actions::update_bill(&mut bills),
            None => break,
        }
    }
    None
}

fn main() {
    run_program();
}
