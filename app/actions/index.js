import fetch from 'isomorphic-fetch';
import { pushState } from 'redux-router';
import { Promise } from 'es6-promise';

export const ERR_HAPPENED = "ERR_HAPPENED";
export const ERR_DELETE = "ERR_DELETE";

export function errHappened(message) {
    return {
        type: ERR_HAPPENED,
        message
    };
}

export function deleteError() {
    return {
        type: ERR_DELETE
    };
}

export function responseHandler(response) {
    return Promise.resolve(response).
        then(response => response.json())
        .then(json => {
            if ((typeof(json) !== 'object') || !('status' in json)) {
                return json;
            }
            throw new AppError(json.message, json.status);
        });
}


class AppError extends Error {
    constructor(message, status) {
        super(message);
        this.message = message;
        this.status = status;
        this.err_name = 'AppError';
    }
}
