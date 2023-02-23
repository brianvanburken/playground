use std::collections::HashMap;

use tracing::{event, instrument, Level};
use warp::http::StatusCode;

use crate::profanity::check_profanity;
use crate::store::Store;
use crate::types::account::Session;
use crate::types::pagination::{extract_pagination, Pagination};
use crate::types::question::{NewQuestion, Question};

#[instrument]
pub async fn get_questions(
    params: HashMap<String, String>,
    store: Store,
) -> Result<impl warp::Reply, warp::Rejection> {
    event!(target: "rust_web_development", Level::INFO, "querying questions");
    let pagination = if !params.is_empty() {
        event!(Level::INFO, pagination = true);
        extract_pagination(params)?
    } else {
        Pagination::default()
    };

    store
        .get_questions(pagination.limit, pagination.offset)
        .await
        .map(|res| warp::reply::json(&res))
        .map_err(warp::reject::custom)
}

pub async fn update_question(
    id: i32,
    session: Session,
    store: Store,
    question: Question,
) -> Result<impl warp::Reply, warp::Rejection> {
    let account_id = session.account_id;
    if !store.is_question_owner(id, &account_id).await? {
        return Err(warp::reject::custom(handle_errors::Error::Unauthorized));
    }
    let (title, content) = tokio::join!(
        check_profanity(question.title),
        check_profanity(question.content),
    );
    let title = title.map_err(warp::reject::custom)?;
    let content = content.map_err(warp::reject::custom)?;

    let question = Question {
        id: question.id,
        tags: question.tags,
        title,
        content,
    };

    store
        .update_question(question, id, account_id)
        .await
        .map(|res| warp::reply::json(&res))
        .map_err(warp::reject::custom)
}

pub async fn delete_question(
    id: i32,
    session: Session,
    store: Store,
) -> Result<impl warp::Reply, warp::Rejection> {
    let account_id = session.account_id;
    if !store.is_question_owner(id, &account_id).await? {
        return Err(warp::reject::custom(handle_errors::Error::Unauthorized));
    }
    store
        .delete_question(id, account_id)
        .await
        .map(|_| warp::reply::with_status(format!("Question {id} deleted"), StatusCode::OK))
        .map_err(warp::reject::custom)
}

pub async fn add_question(
    session: Session,
    store: Store,
    new_question: NewQuestion,
) -> Result<impl warp::Reply, warp::Rejection> {
    let (title, content) = tokio::join!(
        check_profanity(new_question.title),
        check_profanity(new_question.content),
    );
    let title = title.map_err(warp::reject::custom)?;
    let content = content.map_err(warp::reject::custom)?;

    let question = NewQuestion {
        title,
        content,
        tags: new_question.tags,
    };

    store
        .add_question(question, session.account_id)
        .await
        .map(|_| warp::reply::with_status("Question added", StatusCode::OK))
        .map_err(warp::reject::custom)
}
