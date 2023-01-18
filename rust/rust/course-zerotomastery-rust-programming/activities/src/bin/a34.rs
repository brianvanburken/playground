// Topic: Typestates
//
// Summary:
//   An airline wants to reduce the amount of lost luggage by
//   ensuring luggage is properly tracked.
//
// Requirements:
// * Implement a luggage tracking system using the typestate pattern
// * Each piece of luggage has a tracking id
// * Luggage goes through multiple states at the airport:
//   * Check-in        (passenger gives luggage to airport)
//   * OnLoading       (luggage is loaded onto correct plane)
//   * Offloading      (luggage is taken off plane at destination)
//   * AwaitingPickup  (luggage is at destination waiting for passenger pickup)
//   * EndCustody      (luggage was picked up by passenger)
// Notes:
// * Optionally use generics for each state

#[derive(Debug)]
struct Luggage<State> {
    id: usize,
    state: State,
}

#[derive(Debug)]
struct CheckedIn;
#[derive(Debug)]
struct OnLoading;
#[derive(Debug)]
struct OffLoading;
#[derive(Debug)]
struct AwaitingPickup;
#[derive(Debug)]
struct EndCustody;

impl Luggage<CheckedIn> {
    fn new(id: usize) -> Self {
        Self {
            id,
            state: CheckedIn,
        }
    }

    fn load_on_plane(self) -> Luggage<OnLoading> {
        self.transition(OnLoading)
    }
}

impl Luggage<OnLoading> {
    fn load_off_plane(self) -> Luggage<OffLoading> {
        self.transition(OffLoading)
    }
}

impl Luggage<OffLoading> {
    fn put_on_conveyer(self) -> Luggage<AwaitingPickup> {
        self.transition(AwaitingPickup)
    }
}

impl Luggage<AwaitingPickup> {
    fn picked_up(self) -> Luggage<EndCustody> {
        self.transition(EndCustody)
    }
}

impl<State> Luggage<State> {
    fn transition<NextState>(self, state: NextState) -> Luggage<NextState> {
        Luggage { id: self.id, state }
    }
}

fn main() {
    let luggage = Luggage::new(1)
        .load_on_plane()
        .load_off_plane()
        .put_on_conveyer()
        .picked_up();
    dbg!(luggage);
}
