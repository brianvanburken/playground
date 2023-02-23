use std::future;

use crate::store::Store;
use crate::types::account::{Account, AccountId, Session};
use argon2::{self, Config};
use chrono::prelude::*;
use rand::Rng;
use warp::http::StatusCode;
use warp::Filter;

pub async fn register(store: Store, account: Account) -> Result<impl warp::Reply, warp::Rejection> {
    let hashed_password = hash_password(account.password.as_bytes());
    let account = Account {
        id: account.id,
        email: account.email,
        password: hashed_password,
    };
    store
        .add_account(account)
        .await
        .map(|_| warp::reply::with_status("Account added", StatusCode::OK))
        .map_err(warp::reject::custom)
}

pub async fn login(store: Store, login: Account) -> Result<impl warp::Reply, warp::Rejection> {
    let account = store.get_account(login.email).await?;
    let verified = verify_password(&account.password, login.password.as_bytes())
        .map_err(handle_errors::Error::ArgonLibraryError)?;
    if verified {
        let token = issue_token(account.id.expect("id not found"));
        Ok(warp::reply::json(&token))
    } else {
        Err(handle_errors::Error::WrongPassword).map_err(warp::reject::custom)
    }
}

pub fn hash_password(password: &[u8]) -> String {
    let salt = rand::thread_rng().gen::<[u8; 32]>();
    let config = Config::default();
    argon2::hash_encoded(password, &salt, &config).unwrap()
}

fn verify_password(hash: &str, password: &[u8]) -> Result<bool, argon2::Error> {
    argon2::verify_encoded(hash, password)
}

const ENCRYPTION_KEY: &[u8] = "RANDOM WORDS WINTER MACINTOSH PC".as_bytes();

fn issue_token(account_id: AccountId) -> String {
    let current_date_time = Utc::now();
    let dt = current_date_time + chrono::Duration::days(1);
    paseto::tokens::PasetoBuilder::new()
        .set_encryption_key(&Vec::from(ENCRYPTION_KEY))
        .set_expiration(&dt)
        .set_not_before(&Utc::now())
        .set_claim("account_id", serde_json::json!(account_id))
        .build()
        .expect("Failed to construct paseto token w/ builder!")
}

pub fn verify_token(token: String) -> Result<Session, handle_errors::Error> {
    paseto::tokens::validate_local_token(
        &token,
        None,
        ENCRYPTION_KEY,
        &paseto::tokens::TimeBackend::Chrono,
    )
    .and_then(|token| Ok(serde_json::from_value::<Session>(token)?))
    .map_err(|_| handle_errors::Error::CannotDecryptToken)
}

pub fn auth() -> impl Filter<Extract = (Session,), Error = warp::Rejection> + Clone {
    warp::header::<String>("Authorization").and_then(|token: String| {
        let result = verify_token(token).map_err(|_| warp::reject::reject());
        future::ready(result)
    })
}
