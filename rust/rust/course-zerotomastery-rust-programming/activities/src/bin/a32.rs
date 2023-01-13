// Topic: Lifetimes & Structures
//
// Requirements:
// * Display just the names and titles of persons from the mock-data.csv file
// * The names & titles must be stored in a struct separately from the mock
//   data for potential later usage
// * None of the mock data may be duplicated in memory
//
// Notes:
// * The mock data has already been loaded with the include_str! macro, so all functionality
//   must be implemented using references/borrows

const MOCK_DATA: &'static str = include_str!("mock-data.csv");

#[derive(Debug)]
struct Person<'a> {
    first_name: &'a str,
    title: &'a str,
}

impl<'a> From<&'a str> for Person<'a> {
    fn from(s: &'a str) -> Self {
        let data: Vec<&str> = s.split(",").collect();
        match data[..] {
            [_, first_name, _, _, title] => Person { first_name, title },
            _ => panic!("Expected uniform CSV line"),
        }
    }
}

fn main() {
    MOCK_DATA
        .lines()
        .skip(1) // Skip header
        .map(Person::from)
        .for_each(|p| println!("Person: name {} and title {}", p.first_name, p.title));
}
