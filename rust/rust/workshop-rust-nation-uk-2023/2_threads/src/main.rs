use std::sync::mpsc::{channel, Sender};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

type Task = Box<dyn FnOnce() + Send + 'static>;

struct ThreadPool {
    sender: Sender<Task>,
    handles: Vec<JoinHandle<()>>,
}

impl ThreadPool {
    fn new(number_of_threads: u8) -> Self {
        let (sender, receiver) = channel::<Task>();
        let receiver = Arc::new(Mutex::new(receiver));

        let mut handles = vec![];
        for _ in 0..number_of_threads {
            let receiver_cloned = receiver.clone();
            handles.push(thread::spawn(move || loop {
                let message = receiver_cloned.lock().unwrap().recv();
                match message {
                    Ok(task) => task(),
                    Err(_) => break,
                }
            }))
        }

        Self { sender, handles }
    }

    fn execute<T>(&self, task: T)
    where
        T: FnOnce() + Send + 'static,
    {
        self.sender.send(Box::new(task)).expect("Expected a result");
    }

    fn stop(self) {
        drop(self.sender);
        for handle in self.handles {
            handle
                .join()
                .expect("Expected the thread to successfully ended");
        }
    }
}

fn main() {
    let pool = ThreadPool::new(10);

    pool.execute(|| {
        thread::sleep(Duration::from_secs(2));
        println!("SLOW Hello from thread");
    });
    for i in 0..15 {
        pool.execute(move || {
            println!("FAST Hello from thread for task: {}", i);
        });
    }

    // First we're making sure enough time is given to threads to execute the tasks
    // Then, replace this line with the `stop` method.
    // thread::sleep(Duration::from_secs(3));
    pool.stop();
}
