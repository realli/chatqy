import { errHappened } from '../actions'
import { authed_fetch } from '../actions/auth'

export const SOCKET_CONNECTED = "SOCKET_CONNECTED";
export const SOCKET_CLOSED= "SOCKET_CLOSED";

export const CHAT_MESSAGE_SEND = "CHAT_MESSAGE_SEND";
export const CHAT_MESSAGE_RECEIVED = "CHAT_MESSAGE_RECEIVED";
export const REPLACE_ROOM_LIST = "REPLACE_ROOM_LIST";
export const UPDATE_ROOM_LIST_ADD = "UPDATE_ROOM_LIST_ADD";
export const UPDATE_ROOM_LIST_DEL = "UPDATE_ROOM_LIST_DEL";
export const CHANGE_CURRENT_ROOM = "CHANGE_CURRENT_ROOM";

export const RECEIVE_TEXT_MSG = "RECEIVE_TEXT_MSG";
export const RECEIVE_JOIN_MSG = "RECEIVE_JOIN_MSG";
export const RECEIVE_LEAVE_MSG = "RECEIVE_LEAVE_MSG";
export const RECEIVE_ERROR_MSG = "RECEIVE_ERROR_MSG";

function connectEstablished(socket) {
    return {
        type: SOCKET_CONNECTED,
        socket
    };
}

function connectionClosed(reason) {
    return {
        type: SOCKET_CLOSED,
    };
}

export function initConnect() {
    // create new websocket things
    // and try to clean the old ones
    //var socket = WebSocket('ws://som')
    return function(dispatch, getState) {
        let state = getState();
        let connected = state.chat.connected;
        if (connected) {
            return;
        }

        let access_token = state.auth.access_token;
        let socket  = new WebSocket("ws://" + location.host);

        socket.onclose = function(e) {
            console.log(e);
            dispatch(connectionClosed());
        };
        socket.onerror = function(e) {
            console.log(e);
            dispatch(connectionClosed());
        }

        socket.onopen = function() {
            socket.onmessage = function(e) {
                let msg = JSON.parse(e.data);
                console.log(msg);
                dispatch(receiveMsg(msg));
            }
            socket.send(access_token);
            dispatch(connectEstablished(socket));
        };
    }
}

function receiveMsg(msg) {
    switch(msg.type) {
        case 'join':
            return {
                type: RECEIVE_JOIN_MSG,
                msg
            }
        case 'leave':
            return {
                type: RECEIVE_LEAVE_MSG,
                msg
            }
        case 'msg':
            return {
                type: RECEIVE_TEXT_MSG,
                msg
            }
        case 'error':
            return {
                type: RECEIVE_ERROR_MSG,
                msg
            }
        default:
            console.log(msg);
            break;
    }
}

export function enterRoom(roomname) {
    return function(dispatch, getState) {
        let state = getState();
        let connected = state.chat.connected;
        if (!connected) {
            return dispatch(connectionClosed());
        }
        let socket = state.chat.socket;

        let json = {
            type: 'join',
            roomname: roomname
        };
        socket.send(JSON.stringify(json));
        dispatch(changeCurrentRoom(roomname));
    };
}

export function sendMsg(roomName, msg) {
    return function(dispatch, getState){
        let state = getState();
        let connected = state.chat.connected;
        if (!connected) {
            // only one socket is allowed
            return dispatch(connectionClosed());
        }
        let socket = state.chat.socket;

        let json = {
            type: 'msg',
            roomname: roomName,
            payload: msg
        };
        socket.send(JSON.stringify(json));
    }
}

function replaceRoomList(room_list){
    return {
        type: REPLACE_ROOM_LIST,
        room_list
    };
}

function updateRoomListAdd(room){
    return {
        type: UPDATE_ROOM_LIST_ADD,
        room
    };
}

function updateRoomListDel(room){
    return {
        type: UPDATE_ROOM_LIST_DEL,
        room
    };
}

export function getRoomList() {
    return function(dispatch) {
        return dispatch(authed_fetch('/api/rooms')).then(json => {
            dispatch(replaceRoomList(json));
        }).catch(e => {
            console.error(e);
            dispatch(errHappened(e.message));
        });
    }
}

function touchRoom(roomname, description) {
    return function(dispatch){
        return dispatch(authed_fetch('/api/rooms', {
            method: 'post',
            body: JSON.stringify({
                name: roomname,
                description
            })
        })).catch(e => {
            console.error(e);
            dispatch(errHappened(e.message));
        });
    }
}

export function touchAndJoinRoom(roomname, description) {
    return function(dispatch) {
        dispatch(touchRoom(roomname, description)).then(() => {
            dispatch(joinInRoom(roomname));
        });
    };
}

export function joinInRoom(roomname) {
    return function(dispatch) {
        return dispatch(authed_fetch('/api/room/' + roomname + "/join", {
            method: 'post',
            body: ''
        })).then(json => {
            dispatch(updateRoomListAdd(json));
        }).catch(e => {
            console.error(e);
            dispatch(errHappened(e.message));
        });
    };
}

export function leaveRoom(roomname) {
    return function(dispatch) {
        return dispatch(authed_fetch('/api/room/' + roomname, {
            method: 'delete'
        })).then(json => {
            dispatch(updateRoomListDel(json));
        }).catch(e => {
            console.error(e);
            dispatch(errHappened(e.message));
        });
    };

}

function changeCurrentRoom(roomname) {
    return {
        type: CHANGE_CURRENT_ROOM,
        roomname
    };
}

