// threads1.rs
// Execute `rustlings hint threads1` or use the `hint` watch subcommand for a hint.
// This program should wait until all the spawned threads have finished before exiting.

use std::thread;
use std::time::Duration;

fn main() {
    let mut handles = vec![];
    for i in 0..10 {
        let join_handle: thread::JoinHandle<_> = thread::spawn(move || {
            thread::sleep(Duration::from_millis(250));
            println!("thread {} is complete", i);
        });
        handles.push(join_handle);
    }

    let mut completed_threads = 0;
    for handle in handles {
        handle.join().unwrap();
        completed_threads += 1;
    }

    if completed_threads != 10 {
        panic!("Oh no! All the spawned threads did not finish!");
    }
}
