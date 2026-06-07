use std::sync::Arc;

fn main() {
    println!("Hello, world!");
}

pub trait DrinkMaker {
    fn make_drink(&self, command: String);
}

pub struct Machine {
    pub drink_maker: Arc<dyn DrinkMaker>,
}

impl Machine {
    pub fn new(drink_maker: Arc<dyn DrinkMaker>) -> Self {
        Self { drink_maker }
    }

    pub fn make_beverage(&self, order: Order) {
        let command = match order.beverage {
            Beverage::Tea => "T::".to_string(),
            Beverage::Coffee => "C::".to_string(),
            Beverage::Chocolate => "H::".to_string(),
        };

        self.drink_maker.make_drink(command)
    }
}

pub struct Order {
    pub beverage: Beverage,
    pub sugar: Sugar,
}

pub enum Beverage {
    Tea,
    Coffee,
    Chocolate,
}

pub enum Sugar {
    None,
}

#[cfg(test)]
mod tests {
    use rstest::rstest;
    use std::cell::RefCell;

    use crate::*;

    pub struct DrinkMakerSpy {
        pub commands: RefCell<Vec<String>>,
    }

    impl DrinkMakerSpy {
        pub fn new() -> Self {
            Self {
                commands: RefCell::new(vec![]),
            }
        }

        pub fn get_last_command(&self) -> Option<String> {
            self.commands.borrow().iter().last().cloned()
        }
    }

    impl DrinkMaker for DrinkMakerSpy {
        fn make_drink(&self, command: String) {
            self.commands.borrow_mut().push(command)
        }
    }
    // empty path?
    #[rstest]
    #[case(Beverage::Tea, "T::")]
    #[case(Beverage::Coffee, "C::")]
    #[case(Beverage::Chocolate, "H::")]
    fn it_makes_beverage_without_sugar_and_stick(
        #[case] input: Beverage,
        #[case] expected_command: &str,
    ) {
        // Given
        let drink_maker_spy = Arc::new(DrinkMakerSpy::new());
        let order = Order {
            beverage: input,
            sugar: Sugar::None,
        };
        let machine_instance = Machine::new(drink_maker_spy.clone());

        // When
        machine_instance.make_beverage(order);

        // Then
        assert_eq!(
            expected_command,
            drink_maker_spy.get_last_command().unwrap()
        );
    }

    // it_makes_tea_with_one_sugar_and_one_stick
    // it_makes_coffee_with_one_sugar_and_one_stick
    // it_makes_chocolate_with_one_sugar_and_one_stick
    // it_makes_tea_with_two_sugar_and_one_stick
    // it_makes_coffee_with_two_sugar_and_one_stick
    // it_makes_chocolate_with_two_sugar_and_one_stick
    // it
}
