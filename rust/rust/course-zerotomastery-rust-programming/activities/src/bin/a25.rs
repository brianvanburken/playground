// Topic: Traits
//
// Requirements:
// * Calculate the perimeter of a square and triangle:
//   * The perimeter of a square is the length of any side*4.
//   * The perimeter of a triangle is a+b+c where each variable
//     represents the length of a side.
// * Print out the perimeter of the shapes
//
// Notes:
// * Use a trait to declare a perimeter calculation function
// * Use a single function to print out the perimeter of the shapes
//   * The function must utilize impl trait as a function parameter

trait Perimiter {
    fn calculate_perimiter(&self) -> f64;
}

struct Square {
    side: f64,
}

impl Perimiter for Square {
    fn calculate_perimiter(&self) -> f64 {
        self.side * 4.0
    }
}

struct Triangle {
    side_a: f64,
    side_b: f64,
    side_c: f64,
}

impl Perimiter for Triangle {
    fn calculate_perimiter(&self) -> f64 {
        self.side_a + self.side_b + self.side_c
    }
}

fn get_perimiter(obj: impl Perimiter) -> f64 {
    obj.calculate_perimiter()
}

fn main() {
    println!("perimeter = {:?}", get_perimiter(Square { side: 5.0 }));
    println!(
        "perimeter = {:?}",
        get_perimiter(Triangle {
            side_a: 5.0,
            side_b: 3.0,
            side_c: 7.0
        })
    );
}
