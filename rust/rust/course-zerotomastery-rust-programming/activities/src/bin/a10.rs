// Topic: Working with expressions
//
// Requirements:
// * Print "its big" if a variable is > 100
// * Print "its small" if a variable is <= 100
//
// Notes:
// * Use a boolean variable set to the result of
//   an if..else expression to store whether the value
//   is > 100 or <= 100
// * Use a function to print the messages
// * Use a match expression to determine which message
//   to print

fn main() {
    let value = 5;
    let is_gt_than_100 = value > 100;
    display_result(is_gt_than_100)
}

fn display_result(result: bool) {
    match result {
        true => println!("its big"),
        false => println!("its small"),
    }
}
