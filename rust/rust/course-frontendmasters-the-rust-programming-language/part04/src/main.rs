fn main() {
    let mut city_names = vec!["Pythonia", "Javasburg", "C by the Sea", "Rustville"];

    let last_city = match city_names.pop() {
        Some(value) => value,
        None => "",
    };

    if last_city.starts_with("R") {
        println!("“{}” starts with an R!", last_city);
    } else {
        println!("“{}” doesn't start with R", last_city);
    }

    city_names.push(last_city);

    println!("Here is the full list of cities:");
    // 👉 TODO print each of the city names.
    //
    // 💡 TIP: Here's an example of `for` loop syntax:
    //
    //     for my_element in my_vec.iter() { ... }
    for city_name in city_names.iter() {
        println!("{}", city_name);
    }
}
