// Topic: HashMap
//
// Requirements:
// * Print the name and number of items in stock for a furniture store
// * If the number of items is 0, print "out of stock" instead of 0
// * The store has:
//   * 5 Chairs
//   * 3 Beds
//   * 2 Tables
//   * 0 Couches
// * Print the total number of items in stock
//
// Notes:
// * Use a HashMap for the furniture store stock

use std::collections::HashMap;

fn main() {
    let mut total = 0;
    let mut stock = HashMap::new();
    stock.insert("Chair", 5);
    stock.insert("Bed", 3);
    stock.insert("Table", 2);
    stock.insert("Couch", 0);

    for (item, quantity) in stock.iter() {
        total = total + quantity;
        let stock_count = if quantity == &0 {
            "out_of_stock".to_owned()
        } else {
            quantity.to_string()
        };
        println!("item={:?}, stock={:?}", item, stock_count);
    }
    println!("total stock={:?}", total);
}
