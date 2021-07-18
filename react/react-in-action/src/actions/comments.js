import * as types from '../constants/types';
import * as API from '../shared/http';
import { createError } from './error';

/**
 * Show comments for a post
 * @method showComments

 * @param  {string}     postId post id
 * @return {object}
 */
export function showComments(postId) {
    return {
        type: types.comments.SHOW,
        postId
    };
}

/**
 * Toggle comments for a post open or closed
 * @method toggleComments

 * @param  {string}       postId post id to toggle comments for
 * @return {object}
 */
export function toggleComments(postId) {
    return {
        type: types.comments.TOGGLE,
        postId
    };
}

/**
 * Update what comments are available to the app
 * @method updateAvailableComments

 * @param  {Array<object>}                comments incoming comments
 * @return {object}
 */
export function updateAvailableComments(comments) {
    return {
        type: types.comments.GET,
        comments
    };
}

/**
 * Create a comment
 * @method createComment

 * @param  {object}      payload comment payload
 * @return {object}
 */
export function createComment(payload) {
    return dispatch => {
        return API.createComment(payload)
            .then(res => res.json())
            .then(comment => {
                dispatch({
                    type: types.comments.CREATE,
                    comment
                });
            })
            .catch(err => dispatch(createError(err)));
    };
}

/**
 * Load the comments for a particular post
 * @method getCommentsForPost

 * @param  {string}           postId post id to load for
 * @return {object}
 */
export function getCommentsForPost(postId) {
    return dispatch => {
        return API.fetchCommentsForPost(postId)
            .then(res => res.json())
            .then(comments => dispatch(updateAvailableComments(comments)))
            .catch(err => dispatch(createError(err)));
    };
}
