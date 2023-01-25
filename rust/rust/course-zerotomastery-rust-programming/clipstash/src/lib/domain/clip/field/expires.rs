use crate::domain::clip::ClipError;
use crate::domain::time::Time;
use rocket::form::{self, FromFormField, ValueField};
use serde::{Deserialize, Serialize};
use std::str::FromStr;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Expires(Option<Time>);

impl Expires {
    pub fn new<T: Into<Option<Time>>>(expires: T) -> Self {
        Self(expires.into())
    }

    pub fn into_inner(self) -> Option<Time> {
        self.0
    }
}

impl Default for Expires {
    fn default() -> Self {
        Self::new(None)
    }
}

impl FromStr for Expires {
    type Err = ClipError;
    fn from_str(raw: &str) -> Result<Self, Self::Err> {
        if raw.is_empty() {
            Ok(Self(None))
        } else {
            Time::from_str(raw)
                .map(|time| Self::new(time))
                .map_err(|e| e.into())
        }
    }
}

#[rocket::async_trait]
impl<'r> FromFormField<'r> for Expires {
    fn from_value(field: ValueField<'r>) -> form::Result<'r, Self> {
        if field.value.trim().is_empty() {
            Ok(Self(None))
        } else {
            Ok(Self::from_str(field.value)
                .map_err(|e| form::Error::validation(format!("{}", e)))?)
        }
    }
}
