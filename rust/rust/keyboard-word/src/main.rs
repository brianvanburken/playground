const QWERTY: [&str; 3] = [
    "qwertyuiopQWERTYUIOP",
    "asdfghjklASDFGHJKL",
    "zxcvbnmZXCVBNM",
];

fn is_keyboard_row_word(word: &str) -> bool {
    for row in QWERTY {
        if word.chars().all(|c| row.contains(c)) {
            return true;
        }
    }
    false
}

fn main() {
    let words = vec![
        "Hello".to_string(),
        "Alaska".to_string(),
        "Dad".to_string(),
        "Peace".to_string(),
    ];
    let keyboard_words: Vec<String> = words
        .into_iter()
        .filter(|w| is_keyboard_row_word(w))
        .collect();
    println!("{:?}", keyboard_words); // Output: ["Alaska", "Dad"]
}
