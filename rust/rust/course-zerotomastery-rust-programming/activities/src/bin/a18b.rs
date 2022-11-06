// Topic: Result & the question mark operator
//
// Requirements:
// * Determine if an employee can access a building using a digital keycard
// * Employees that can access the building are:
//   * Maintenance crews
//   * Marketing department employees
//   * Managers
// * Other employees that work at the company are:
//   * Line supervisors
//   * Kitchen staff
//   * Assembly technicians
// * Ensure that terminated employees cannot access the building
//   regardless of their position
//
// Notes:
// * Use an enum to represent all types of employees
// * Use a struct to store the employee type and whether they are
//   still employed
// * Use a function that returns a Result to determine if the employee
//   may enter the building
// * Print whether the employee may access the building
//   * Must use a function that utilizes the question mark operator to do this

enum Department {
    MaintenanceCrew,
    Marketing,
    Manager,
    LineSupervisor,
    KitchenStaff,
    AssemblyTechnician,
}

enum Employment {
    Active,
    Terminated,
}

struct Employee {
    department: Department,
    active_employment: Employment,
}

fn try_enter_building(employee: &Employee) -> Result<(), String> {
    match employee {
        Employee {
            active_employment: Employment::Terminated,
            ..
        } => Err("Need to have active employment".to_owned()),
        Employee {
            department: Department::MaintenanceCrew,
            ..
        }
        | Employee {
            department: Department::Marketing,
            ..
        }
        | Employee {
            department: Department::Manager,
            ..
        } => Ok(()),
        _ => Err("Employee does not have access to building".to_owned()),
    }
}

fn print_access(employee: &Employee) -> Result<(), String> {
    try_enter_building(employee)?;
    println!("Access OK");
    Ok(())
}

fn main() {
    let bob = Employee {
        department: Department::Manager,
        active_employment: Employment::Terminated,
    };
    println!("{:?}", print_access(&bob));
}
