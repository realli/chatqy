import {ERR_HAPPENED,
    ERR_DELETE} from '../actions';

const initialState = {
    messages: []
};

function errMsgR(state=initialState, action) {
    switch (action.type) {
        case ERR_HAPPENED:
            return Object.assign({}, state, {
                messages: state.messages.concat([action.message])
            });
        case ERR_DELETE:
            return Object.assign({}, state, {
                messages: []
            });

        default:
            return state;
    }

}

export default errMsgR;
