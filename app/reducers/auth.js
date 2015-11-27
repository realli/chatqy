import {ERR_HAPPENED,
    ERR_DELETE,
    USER_LOGOUT,
    USER_LOGIN,
    USER_LOGIN_FAILED,
    USER_LOGIN_SUCCESS,
    USER_SIGNUP,
    USER_SIGNUP_FAILED,
    USER_SIGNUP_SUCCESS} from '../actions/auth';

const initialState = {
    is_login: false,
    login_pending: false,
    signup_pending: false,
    user: {
        name: ''
    },
    access_token: '',
    refresh_token: '',
    expire_at: new Date(),
};

function authR(state=initialState, action) {
    switch (action.type) {
        case USER_LOGIN:
            return Object.assign({}, state, {
                is_login: false,
                login_pending: true,
                user: action.user
            });
        case USER_LOGIN_FAILED:
            return Object.assign({}, state, {
                is_login: false,
                login_pending: false,
            });
        case USER_SIGNUP:
            return Object.assign({}, state, {
                signup_pending: true,
                user: action.user
            });
        case USER_SIGNUP_FAILED:
            return Object.assign({}, state, {
                signup_pending: false,
            });
        case USER_LOGIN_SUCCESS:
        case USER_SIGNUP_SUCCESS:
            let current = new Date();
            current.setSeconds(current.getSeconds() + action.expiration);
            return Object.assign({}, state, {
                is_login: true,
                login_pending: false,
                signup_pending: false,
                access_token: action.access_token,
                refresh_token: action.refresh_token,
                expire_at: current
            });
        case USER_LOGOUT:
            return Object.assign({}, state, {
                is_login: false,
                login_pending: false,
                signup_pending: false,
                access_token: '',
                refresh_token: ''
            });
        default:
            return state;
    }

}

export default authR;
