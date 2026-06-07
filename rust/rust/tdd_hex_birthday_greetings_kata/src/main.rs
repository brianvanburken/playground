use chrono::*;
use mockall::predicate::*;
use mockall::*;
use std::io::Write;
use std::sync::Arc;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

mod email_notifier;
mod friends_flat_file;

fn main() {
    println!("Hello, world!");
}

#[automock]
trait Notifier {
    fn notify(&self, _greeting: Greeting);
}

#[automock]
trait Friends {
    fn list_friends(&self) -> Vec<Friend>;
}

struct Greeter {
    notifier: Arc<dyn Notifier>,
    friends: Arc<dyn Friends>,
    calendar: Arc<dyn Calendar>,
}

#[derive(Debug, PartialEq, Eq)]
struct Greeting {
    fullname: String,
    email: String,
}

impl Greeting {
    pub fn new(fullname: String, email: String) -> Self {
        Self { fullname, email }
    }
}

impl Greeter {
    pub fn new(
        notifier: Arc<dyn Notifier>,
        friends: Arc<dyn Friends>,
        calendar: Arc<dyn Calendar>,
    ) -> Self {
        Self {
            notifier,
            friends,
            calendar,
        }
    }

    pub fn send_greetings(&self) {
        let today = self.calendar.today();
        for friend in self
            .friends
            .list_friends()
            .into_iter()
            .filter(|f| f.has_birthday(today))
        {
            self.notifier.notify(Greeting::from(friend));
        }
    }
}

impl From<Friend> for Greeting {
    fn from(friend: Friend) -> Self {
        Self {
            fullname: format!("{} {}", friend.first_name, friend.last_name),
            email: friend.email,
        }
    }
}

#[automock]
trait Calendar {
    fn today(&self) -> NaiveDate;
}

#[derive(Debug, PartialEq, Eq)]
struct Friend {
    first_name: String,
    last_name: String,
    email: String,
    birthday: NaiveDate,
}
impl Friend {
    pub fn new(last_name: String, first_name: String, email: String, birthday: NaiveDate) -> Self {
        Self {
            last_name,
            first_name,
            email,
            birthday,
        }
    }

    pub fn has_birthday(&self, today: NaiveDate) -> bool {
        self.birthday.day0() == today.day0() && self.birthday.month0() == today.month0()
    }
}

#[cfg(test)]
mod tests {
    use tempfile::NamedTempFile;

    use crate::*;
    use std::sync::Arc;

    use self::{
        email_notifier::{Email, EmailNotifier, MockMailer},
        friends_flat_file::FriendsFlatFile,
    };

    #[test]
    fn sends_single_birthday_greeting() {
        //Given
        let mut mock_friends = MockFriends::new();

        mock_friends.expect_list_friends().returning(|| {
            vec![Friend::new(
                "Paese".to_string(),
                "Andrea".to_string(),
                "andrea.paese@prima.it".to_string(),
                NaiveDate::from_ymd_opt(2024, 01, 22).unwrap(),
            )]
        });

        let mut mock_notifer = MockNotifier::new();
        mock_notifer
            .expect_notify()
            .with(eq(Greeting::new(
                "Andrea Paese".to_string(),
                "andrea.paese@prima.it".to_string(),
            )))
            .times(1)
            .return_const(());

        let today = NaiveDate::from_ymd_opt(2024, 01, 22).unwrap();
        let mut mock_calendar = MockCalendar::new();
        mock_calendar.expect_today().return_const(today);

        let calendar = Arc::new(mock_calendar);
        let friends = Arc::new(mock_friends);
        let notifier = Arc::new(mock_notifer);
        let greeter = Greeter::new(notifier.clone(), friends.clone(), calendar.clone());

        //When
        greeter.send_greetings();
    }

    #[test]
    fn send_no_birthday_greetings() {
        // Given
        let mut mock_friends = MockFriends::new();

        mock_friends.expect_list_friends().returning(|| vec![]);

        let mut mock_notifer = MockNotifier::new();
        mock_notifer.expect_notify().never().return_const(());

        let today = NaiveDate::from_ymd_opt(2024, 01, 22).unwrap();
        let mut mock_calendar = MockCalendar::new();
        mock_calendar.expect_today().return_const(today);

        let calendar = Arc::new(mock_calendar);
        let friends = Arc::new(mock_friends);
        let notifier = Arc::new(mock_notifer);
        let greeter = Greeter::new(notifier.clone(), friends.clone(), calendar.clone());

        //When
        greeter.send_greetings();
    }

    #[test]
    fn send_birthday_greetings_to_people_with_birthday() {
        //Given
        let mut mock_friends = MockFriends::new();

        mock_friends.expect_list_friends().returning(|| {
            vec![
                Friend::new(
                    "Paese".to_string(),
                    "Andrea".to_string(),
                    "andrea.paese@prima.it".to_string(),
                    NaiveDate::from_ymd_opt(2024, 01, 22).unwrap(),
                ),
                Friend::new(
                    "Doe".to_string(),
                    "John".to_string(),
                    "john.doe@prima.it".to_string(),
                    NaiveDate::from_ymd_opt(2024, 01, 22).unwrap(),
                ),
                Friend::new(
                    "Marry".to_string(),
                    "Ann".to_string(),
                    "ann.marry@prima.it".to_string(),
                    NaiveDate::from_ymd_opt(2024, 01, 23).unwrap(),
                ),
            ]
        });
        let mut mock_notifer = MockNotifier::new();
        mock_notifer
            .expect_notify()
            .with(eq(Greeting::new(
                "Andrea Paese".to_string(),
                "andrea.paese@prima.it".to_string(),
            )))
            .times(1)
            .return_const(());

        mock_notifer
            .expect_notify()
            .with(eq(Greeting::new(
                "John Doe".to_string(),
                "john.doe@prima.it".to_string(),
            )))
            .times(1)
            .return_const(());

        let today = NaiveDate::from_ymd_opt(2024, 01, 22).unwrap();
        let mut mock_calendar = MockCalendar::new();
        mock_calendar.expect_today().return_const(today);

        let calendar = Arc::new(mock_calendar);
        let friends = Arc::new(mock_friends);
        let notifier = Arc::new(mock_notifer);
        let greeter = Greeter::new(notifier.clone(), friends.clone(), calendar.clone());

        //When
        greeter.send_greetings();
    }

    #[test]
    fn send_multiple_birthday_greetings() {
        //Given
        let mut mock_friends = MockFriends::new();

        mock_friends.expect_list_friends().returning(|| {
            vec![
                Friend::new(
                    "Paese".to_string(),
                    "Andrea".to_string(),
                    "andrea.paese@prima.it".to_string(),
                    NaiveDate::from_ymd_opt(2024, 01, 22).unwrap(),
                ),
                Friend::new(
                    "Doe".to_string(),
                    "John".to_string(),
                    "john.doe@prima.it".to_string(),
                    NaiveDate::from_ymd_opt(2024, 01, 22).unwrap(),
                ),
            ]
        });

        let mut mock_notifer = MockNotifier::new();
        mock_notifer
            .expect_notify()
            .with(eq(Greeting::new(
                "Andrea Paese".to_string(),
                "andrea.paese@prima.it".to_string(),
            )))
            .times(1)
            .return_const(());

        mock_notifer
            .expect_notify()
            .with(eq(Greeting::new(
                "John Doe".to_string(),
                "john.doe@prima.it".to_string(),
            )))
            .times(1)
            .return_const(());

        let today = NaiveDate::from_ymd_opt(2024, 01, 22).unwrap();
        let mut mock_calendar = MockCalendar::new();
        mock_calendar.expect_today().return_const(today);

        let calendar = Arc::new(mock_calendar);
        let friends = Arc::new(mock_friends);
        let notifier = Arc::new(mock_notifer);
        let greeter = Greeter::new(notifier.clone(), friends.clone(), calendar.clone());

        //When
        greeter.send_greetings();
    }

    // send_birthday_greetings_for_feb_29th_on_feb_28th

    // Acceptance Tests
    #[test]
    fn sends_emails_to_all_friends_having_a_birthday() {
        // Given
        let mut tmp_file = NamedTempFile::new().unwrap();
        writeln!(
            tmp_file,
            r#"last_name, first_name, date_of_birth, email
            Doe, John, 1982/10/08, john.doe@foobar.com
            Giacomello, Damiano, 1970/01/01, damiano.giacomello@foobar.com
            Doe, Anne, 1983/10/08, anne.doe@foobar.com"#
        )
        .unwrap();

        let friends = FriendsFlatFile::new(tmp_file.reopen().unwrap());

        let mut mock_mailer = MockMailer::new();

        mock_mailer
            .expect_send()
            .with(eq(Email::new(
                "Happy birthday!".to_string(),
                "Happy birthday, dear John".to_string(),
                "john.doe@foobar.com".to_string(),
            )))
            .times(1)
            .return_const(());

        mock_mailer
            .expect_send()
            .with(eq(Email::new(
                "Happy birthday!".to_string(),
                "Happy birthday, dear Anne".to_string(),
                "anne.doe@foobar.com".to_string(),
            )))
            .times(1)
            .return_const(());

        let mailer = Arc::new(mock_mailer);
        let email_notifier = EmailNotifier::new(mailer.clone());

        let today = NaiveDate::from_ymd_opt(1982, 10, 08).unwrap();
        let mut mock_calendar = MockCalendar::new();
        mock_calendar.expect_today().return_const(today);

        let calendar = Arc::new(mock_calendar);
        let friends = Arc::new(friends);
        let notifier = Arc::new(email_notifier);
        let greeter = Greeter::new(notifier.clone(), friends.clone(), calendar.clone());

        // When
        greeter.send_greetings();
        // assert 2 email being sent with 3 friends in the file
    }
}
