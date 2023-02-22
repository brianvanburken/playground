use std::collections::HashMap;

use tracing::{event, instrument, Level};
use warp::http::StatusCode;

use crate::store::Store;
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
        .map_err(|e| warp::reject::custom(e))
}

pub async fn update_question(
    id: i32,
    store: Store,
    question: Question,
) -> Result<impl warp::Reply, warp::Rejection> {
    store
        .update_question(question, id)
        .await
        .map(|res| warp::reply::json(&res))
        .map_err(|e| warp::reject::custom(e))
}

pub async fn delete_question(id: i32, store: Store) -> Result<impl warp::Reply, warp::Rejection> {
    store
        .delete_question(id)
        .await
        .map(|_| warp::reply::with_status(format!("Question {} deleted", id), StatusCode::OK))
        .map_err(|e| warp::reject::custom(e))
}

pub async fn add_question(
    store: Store,
    new_question: NewQuestion,
) -> Result<impl warp::Reply, warp::Rejection> {
    store
        .add_question(new_question)
        .await
        .map(|_| warp::reply::with_status("Question added", StatusCode::OK))
        .map_err(|e| warp::reject::custom(e))
}
