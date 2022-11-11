// Topic: Testing
//
// Requirements:
// * Write tests for the existing program to ensure proper functionality.
//
// Notes:
// * Create at least two test cases for each function.
// * Use `cargo test` to test the program.
// * There are intentional bugs in the program that need to be fixed.
// * Check the documentation comments for the functions to
//   determine how the they should operate.

/// Ensures n is >= lower and <= upper.
fn clamp(n: i32, lower: i32, upper: i32) -> i32 {
    if n < lower {
        lower
    } else if n > upper {
        upper
    } else {
        n
    }
}

/// Divides a and b.
fn div(a: i32, b: i32) -> Option<i32> {
    if b == 0 {
        None
    } else {
        Some(a / b)
    }
}

/// Takes two strings and places them immediately one after another.
fn concat(first: &str, second: &str) -> String {
    format!("{}{}", first, second)
}

fn main() {}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn it_clamps_to_lower() {
        assert_eq!(
            clamp(10, 100, 1000),
            100,
            "Expected lower value to be returned"
        );
    }

    #[test]
    fn it_clamps_to_upper() {
        assert_eq!(
            clamp(5000, 10, 1000),
            1000,
            "Expected lower value to be returned"
        );
    }

    #[test]
    fn it_clamps_to_input() {
        let lower = 10;
        let upper = 20;
        assert_eq!(clamp(15, lower, upper), 15, "Expected input to be returned");
    }

    #[test]
    fn it_divides_in_two() {
        assert_eq!(div(10, 2), Some(5), "Expected input divided by two");
    }

    #[test]
    fn it_divides_in_three() {
        assert_eq!(div(9, 3), Some(3), "Expected input divided by three");
    }

    #[test]
    fn it_divides_by_zero() {
        assert_eq!(div(9, 0), None, "Expected none for division by zero");
    }

    #[test]
    fn it_concats_two_strings() {
        assert_eq!(
            concat("1 + ", "1"),
            "1 + 1",
            "Expect two strings to be concated"
        );
    }

    #[test]
    fn it_concats_two_letters() {
        assert_eq!(concat("a", "b"), "ab", "Expect two strings to be concated");
    }
}
