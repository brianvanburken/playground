use crate::*;

pub struct EmailNotifier {
    mailer: Arc<dyn Mailer>,
}

impl EmailNotifier {
    pub fn new(mailer: Arc<dyn Mailer>) -> Self {
        Self { mailer }
    }
}

impl Notifier for EmailNotifier {
    fn notify(&self, greeting: Greeting) {
        let (first_name, _) = greeting.fullname.split_once(' ').unwrap();
        self.mailer.send(Email::new(
            "Happy birthday!".to_string(),
            format!("Happy birthday, dear {first_name}"),
            greeting.email,
        ))
    }
}

#[automock]
pub trait Mailer {
    fn send(&self, email: Email);
}

#[derive(Debug, PartialEq, Eq)]
pub struct Email {
    subject: String,
    email: String,
    body: String,
}

impl Email {
    pub fn new(subject: String, body: String, email: String) -> Self {
        Self {
            subject,
            body,
            email,
        }
    }
}

// Happy path
#[cfg(test)]
mod tests {
    use super::*;

    use super::MockMailer;

    // send_a_greeting_email
    #[test]
    fn send_a_greeting_email() {
        // Given
        let mut mock_mailer = MockMailer::new();

        mock_mailer
            .expect_send()
            .with(eq(Email::new(
                "Happy birthday!".to_string(),
                "Happy birthday, dear Andrea".to_string(),
                "andrea.paese@prima.it".to_string(),
            )))
            .times(1)
            .return_const(());

        let mailer = Arc::new(mock_mailer);
        let email_notifier = EmailNotifier::new(mailer.clone());

        let greeting = Greeting::new(
            "Andrea Paese".to_string(),
            "andrea.paese@prima.it".to_string(),
        );

        //When
        email_notifier.notify(greeting);
    }

    #[test]
    fn send_multiple_greeting_emails() {
        // Given
        let mut mock_mailer = MockMailer::new();

        mock_mailer
            .expect_send()
            .with(eq(Email::new(
                "Happy birthday!".to_string(),
                "Happy birthday, dear John".to_string(),
                "john.doe@prima.it".to_string(),
            )))
            .times(1)
            .return_const(());

        mock_mailer
            .expect_send()
            .with(eq(Email::new(
                "Happy birthday!".to_string(),
                "Happy birthday, dear Mary".to_string(),
                "mary.ann@prima.it".to_string(),
            )))
            .times(1)
            .return_const(());

        let mailer = Arc::new(mock_mailer);
        let email_notifier = EmailNotifier::new(mailer.clone());

        let greeting1 = Greeting::new("John Doe".to_string(), "john.doe@prima.it".to_string());

        let greeting2 = Greeting::new("Mary Ann".to_string(), "mary.ann@prima.it".to_string());

        //When
        email_notifier.notify(greeting1);
        email_notifier.notify(greeting2);
    }
}
