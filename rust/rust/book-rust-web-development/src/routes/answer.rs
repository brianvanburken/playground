use warp::http::StatusCode;

use crate::store::Store;
use crate::types::answer::NewAnswer;

pub async fn add_answer(
    store: Store,
    new_answer: NewAnswer,
) -> Result<impl warp::Reply, warp::Rejection> {
    store
        .add_answer(new_answer)
        .await
        .map(|_| warp::reply::with_status("Answer added", StatusCode::OK))
        .map_err(|e| warp::reject::custom(e))
}
