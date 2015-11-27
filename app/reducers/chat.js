import { UPDATE_ROOM_LIST_ADD,
    REPLACE_ROOM_LIST,
    CHANGE_CURRENT_ROOM,
    UPDATE_ROOM_LIST_DEL,
    SOCKET_CONNECTED,
    SOCKET_CLOSED,
    RECEIVE_JOIN_MSG,
    RECEIVE_TEXT_MSG,
    RECEIVE_LEAVE_MSG
} from '../actions/chat';

const initialState = {
    connected: false,
    socket: undefined,
    currentRoom: undefined,
    roomMap: {}
};

export default function chatR(state=initialState, action) {
    let roomMap = {};
    switch (action.type) {
        case REPLACE_ROOM_LIST:
            action.room_list.map(function(room) {
                roomMap[room.name] = {
                    'description': room.description,
                    'messages': []
                };
            });
            return Object.assign({}, state, {
                roomMap
            });
        case UPDATE_ROOM_LIST_ADD:
            roomMap = Object.assign({}, state.roomMap, {
                [action.room.name]: {
                    description: action.room.description,
                    'messages': []
                }
            });
            return Object.assign({}, state, {
                roomMap
            });
        case UPDATE_ROOM_LIST_DEL:
            roomMap = Object.assign({}, state.roomMap);
            delete roomMap[action.room.name];
            return Object.assign({}, state, {
                roomMap
            });
        case CHANGE_CURRENT_ROOM:
            return Object.assign({}, state, {
                currentRoom: action.roomname
            });
        case SOCKET_CLOSED:
            return Object.assign({}, state, {
                connected: false,
                socket: undefined
            });
        case SOCKET_CONNECTED:
            return Object.assign({}, state, {
                connected: true,
                socket: action.socket
            });
        case RECEIVE_JOIN_MSG:
        case RECEIVE_LEAVE_MSG:
        case RECEIVE_TEXT_MSG:
            roomMap = Object.assign({}, state.roomMap);
            roomMap[action.msg.roomname].messages.push(action.msg);
            return Object.assign({}, state, {
                roomMap
            });
        default:
            return state;
    }

};

