fn main() {
    let numbers = vec![2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24];

    // 💡 TIP: You'll get a compile error. Here are two ways you can fix it:
    //
    // Option 1: Pass numbers.clone() some of the time.
    //           (Experiment to see when it's needed!)
    let sum_of_nums = sum(numbers.clone());
    let product_of_nums = product(numbers.clone());
    let average_of_nums = average(numbers);

    println!("Sum of these numbers: {}", sum_of_nums);
    println!("Product of these numbers: {}", product_of_nums);
    println!("Average of these numbers: {}", average_of_nums);

    // Option 2: Change some of the functions to return a tuple
    //           of (i64, Vec<i64>), using the `numbers` argument
    //           as the Vec<i64> to return. With this approach,
    //           you won't need to call .clone() at all!
    //

    println!("--- More performant way ---->");
    let numbers2 = vec![2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24];

    let (sum_of_nums2, data) = sum2(numbers2);
    let (product_of_nums2, data) = product2(data);
    let average_of_nums2 = average(data);

    println!("Sum of these numbers: {}", sum_of_nums2);
    println!("Product of these numbers: {}", product_of_nums2);
    println!("Average of these numbers: {}", average_of_nums2);
}

fn sum(numbers: Vec<i64>) -> i64 {
    let mut total = 0;

    for num in numbers.iter() {
        total += num;
    }

    total
}

fn sum2(numbers: Vec<i64>) -> (i64, Vec<i64>) {
    let mut total = 0;

    for num in numbers.iter() {
        total += num;
    }

    (total, numbers)
}

fn product(numbers: Vec<i64>) -> i64 {
    let mut total = 1;

    for num in numbers.iter() {
        total *= num;
    }

    total
}

fn product2(numbers: Vec<i64>) -> (i64, Vec<i64>) {
    let mut total = 1;

    for num in numbers.iter() {
        total *= num;
    }

    (total, numbers)
}

fn average(numbers: Vec<i64>) -> i64 {
    let length = numbers.len() as i64;

    sum(numbers) / length
}
