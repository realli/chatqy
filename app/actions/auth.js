import fetch from 'isomorphic-fetch';
import { pushState } from 'redux-router';
import { errHappened, responseHandler } from '../actions'

export const USER_LOGIN = "USER_LOGIN";
export const USER_LOGIN_FAILED = "USER_LOGIN_FAILED";
export const USER_LOGIN_SUCCESS = "USER_LOGIN_SUCCESS";
export const USER_SIGNUP = "USER_SIGNUP";
export const USER_SIGNUP_FAILED = "USER_SIGNUP_FAILED";
export const USER_SIGNUP_SUCCESS = "USER_SIGNUP_SUCCESS";
export const USER_LOGOUT = "USER_LOGOUT";


function login_sending(username) {
    return {
        type: USER_LOGIN,
        user: {
            name: username
        }
    };
}

function login_failed(message) {
    return {
        type: USER_LOGIN_FAILED,
        message
    };
}

function login_success(access_token, refresh_token, expiration){
    return {
        type: USER_LOGIN_SUCCESS,
        access_token,
        refresh_token,
        expiration
    };
}

function signup_failed(message) {
    return {
        type: USER_SIGNUP_FAILED,
        message
    };
}

function signup_sending(username) {
    return {
        type: USER_SIGNUP,
        user: {
            name: username
        }
    };
}

function signup_success(access_token, refresh_token, expiration) {
    return {
        type: USER_SIGNUP_SUCCESS,
        access_token,
        refresh_token,
        expiration
    };
}

function logout_success() {
    return {
        type: USER_LOGOUT
    };
}

export function user_signup(username, password, email) {
    return function (dispatch) {
        dispatch(signup_sending(username));

        return fetch('/api/signup', {
            method: 'post',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                username,
                password,
                email
            }),
            credentials: 'include'
        }).then(responseHandler).then(json => {
                dispatch(signup_success(json.access_token,
                                        json.refresh_token,
                                        json.expiration));
                dispatch(pushState({}, '/'));
        }).catch(e => {
            console.log(e);
            dispatch(signup_failed(e.message));
            dispatch(errHappened(e.message));
        });
    };
}

export function user_login(username, password) {
    return function (dispatch) {
        dispatch(login_sending(username));

        return fetch('/api/login', {
            method: 'post',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                username: username,
                password: password
            }),
            credentials: 'include'
        }).then(responseHandler).then(json => {
            dispatch(login_success(json.access_token,
                                   json.refresh_token,
                                   json.expiration));
            dispatch(pushState({}, '/'));
        }).catch(e => {
            console.log(e);
            dispatch(login_failed(e.message));
            dispatch(errHappened(e.message));
        });
    }
};

export function user_logout() {
    return function (dispatch) {
        return dispatch(authed_fetch('/api/logout'))
               .catch(e => {
                   console.log(e);
               }).then(() => dispatch(logout_success()));
    };
}

function refreshToken() {
    return function(dispatch, getState) {
        const { auth } = getState();
        const {user, refresh_token} = auth;
        const username = user.name;

        return fetch('/api/refresh_token', {
            method: 'post',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                uid: username,
                token: refresh_token
            }),
            credentials: 'include'
        }).then(responseHandler).then(json => {
            dispatch(login_success(json.access_token,
                                   json.refresh_token,
                                   json.expiration));
            return json.access_token;
        });
    }
}

export function authed_fetch(url, options) {
    return function(dispatch, getState) {
        const { auth } = getState();
        const {user, access_token} = auth;
        const username = user.name;

        if(options == null) {
            options = {}
        }
        let new_options = Object.assign({}, options , {
            headers: Object.assign({}, {
                'Accept': 'application/json',
                'Content-Type': 'application/json',
                'Authorization': 'Bearer ' + access_token
            }, options.headers || {})
        });

        return fetch(url, new_options).then(responseHandler).catch(e => {
            if (e.status === 40107) {
                return dispatch(refreshToken()).then(token => {
                    let new_options = Object.assign({}, options , {
                        headers: Object.assign({}, {
                            'Accept': 'application/json',
                            'Content-Type': 'application/json',
                            'Authorization': 'Bearer ' + token
                        }, options.headers || {})
                    });
                    return fetch(url, new_options);
                }).then(responseHandler);
            } else {
                return Promise.reject(e);
            }
        }).catch(e => {
            console.log(e.message);
            dispatch(errHappened(e.message));
            return Promise.reject(e);
        });
    }
}

