use warp::http::StatusCode;

use crate::profanity::check_profanity;
use crate::store::Store;
use crate::types::account::Session;
use crate::types::answer::NewAnswer;

pub async fn add_answer(
    session: Session,
    store: Store,
    new_answer: NewAnswer,
) -> Result<impl warp::Reply, warp::Rejection> {
    let answer = check_profanity(new_answer.content)
        .await
        .map(|content| NewAnswer {
            content,
            question_id: new_answer.question_id,
        })
        .map_err(warp::reject::custom)?;

    store
        .add_answer(answer, session.account_id)
        .await
        .map(|_| warp::reply::with_status("Answer added", StatusCode::OK))
        .map_err(warp::reject::custom)
}
