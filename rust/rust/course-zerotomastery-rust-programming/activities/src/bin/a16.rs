// Topic: Option
//
// Requirements:
// * Print out the details of a student's locker assignment
// * Lockers use numbers and are optional for students
//
// Notes:
// * Use a struct containing the student's name and locker assignment
// * The locker assignment should use an Option<i32>

struct Student {
    name: String,
    locker: Option<i32>,
}

fn main() {
    let bob = Student {
        name: "Bob".to_owned(),
        locker: Some(102),
    };

    if let Some(assignment) = bob.locker {
        println!("{:?}", assignment)
    }
}
