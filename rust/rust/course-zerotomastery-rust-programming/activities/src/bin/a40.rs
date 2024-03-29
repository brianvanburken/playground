// Topic: Smart Pointers & RefCell
//
// Summary:
//   A vehicle rental company wants to access the rentals available
//   at storefront locations. Create a program that provides access
//   to storefront rentals from the corporate headquarters.
//
// Requirements:
// * Corporate must be able to access the rentals at a storefront
// * Storefronts must be able to rent out vehicles
// * Rentals have the following attributes:
//   - Type of vehicle
//   - Vehicle Identification Number (VIN)
//   - Vehicle status:
//     * Available, Unavailable, Maintenance, Rented
//
// Notes:
// * Use Rc and RefCell to create shared mutable data structures
// * Create at least two rentals and ensure that Corporate and StoreFront
//   can both access the rental information
// * Test your program by changing the vehicle status from both a storefront
//   and from corporate

use std::{cell::RefCell, rc::Rc};

#[derive(Debug)]
enum Vehicle {
    Car,
    Truck,
}

#[derive(Debug, Hash, PartialOrd, PartialEq)]
enum Status {
    Available,
    Maintenance,
    Rented,
    Unavailable,
}

#[derive(Debug)]
struct Rental {
    status: Status,
    vehicle: Vehicle,
    vin: String,
}

#[derive(Debug)]
struct Corporate {
    rentals: Rc<RefCell<Vec<Rental>>>,
}

#[derive(Debug)]
struct StoreFront {
    rentals: Rc<RefCell<Vec<Rental>>>,
}

fn main() {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn update_status() {
        let vehicles = vec![
            Rental {
                status: Status::Available,
                vehicle: Vehicle::Car,
                vin: "123".to_owned(),
            },
            Rental {
                status: Status::Maintenance,
                vehicle: Vehicle::Truck,
                vin: "abc".to_owned(),
            },
        ];

        let vehicles = Rc::new(RefCell::new(vehicles));

        let corporate = Corporate {
            rentals: Rc::clone(&vehicles),
        };
        let storefront = StoreFront {
            rentals: Rc::clone(&vehicles),
        };

        {
            let mut rentals = storefront.rentals.borrow_mut();
            if let Some(car) = rentals.get_mut(0) {
                assert_eq!(car.status, Status::Available);
                car.status = Status::Rented;
            }
        }

        {
            let mut rentals = corporate.rentals.borrow_mut();
            if let Some(car) = rentals.get_mut(0) {
                assert_eq!(car.status, Status::Rented);
                car.status = Status::Available;
            }
        }

        let mut rentals = storefront.rentals.borrow();
        if let Some(car) = rentals.get(0) {
            assert_eq!(car.status, Status::Available);
        }
    }

    #[test]
    fn test_borrow_rc() {
        let rentals = Rc::new(RefCell::new(vec![
            Rental {
                status: Status::Available,
                vehicle: Vehicle::Car,
                vin: "123456".to_string(),
            },
            Rental {
                status: Status::Unavailable,
                vehicle: Vehicle::Truck,
                vin: "654321".to_string(),
            },
        ]));
        let corporate = Corporate {
            rentals: rentals.clone(),
        };
        let storefront = StoreFront {
            rentals: rentals.clone(),
        };

        let corporate_rentals = corporate.rentals.borrow();
        let storefront_rentals = storefront.rentals.borrow();

        assert_eq!(corporate_rentals.len(), 2);
        assert_eq!(storefront_rentals.len(), 2);
        assert_eq!(Rc::strong_count(&rentals), 3);
    }
}
