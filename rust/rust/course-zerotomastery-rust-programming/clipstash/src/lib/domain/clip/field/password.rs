use crate::domain::clip::ClipError;
use rocket::form::{self, FromFormField, ValueField};
use serde::{Deserialize, Serialize};
use std::str::FromStr;

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, PartialOrd)]
pub struct Password(Option<String>);

impl Password {
    pub fn new<T: Into<Option<String>>>(password: T) -> Result<Self, ClipError> {
        let password: Option<String> = password.into();
        Ok(Self(password.filter(|p| !p.trim().is_empty())))
    }

    pub fn into_inner(self) -> Option<String> {
        self.0
    }

    pub fn has_password(&self) -> bool {
        self.0.is_some()
    }
}

impl Default for Password {
    fn default() -> Self {
        Self(None)
    }
}

impl FromStr for Password {
    type Err = ClipError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s.to_string())
    }
}

#[rocket::async_trait]
impl<'r> FromFormField<'r> for Password {
    fn from_value(field: ValueField<'r>) -> form::Result<'r, Self> {
        Ok(Self::new(field.value.to_owned())
            .map_err(|e| form::Error::validation(format!("{}", e)))?)
    }
}

#[cfg(test)]
mod test {
    use super::Password;

    #[test]
    fn empty_password_is_none() {
        assert_eq!(false, Password::new("".to_owned()).unwrap().has_password());
    }

    #[test]
    fn default_is_none() {
        assert_eq!(false, Password::default().has_password());
    }

    #[test]
    fn accepts_valid_password() {
        assert!(Password::new("123".to_owned()).unwrap().has_password());
    }
}
