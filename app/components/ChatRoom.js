import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { sendMsg } from '../actions/chat';

import { List,
    ListItem,
    ListSubHeader,
    Input,
    Button } from 'react-toolbox';

class ChatRoom extends Component {
    render() {
        const { dispatch, currentRoom, roomMap } = this.props;
        let messages = [];

        if(currentRoom) {
            messages = roomMap[currentRoom].messages;
            return (
                <div className="pure-g">
                    <div className="pure-u-1">
                        <h1>{currentRoom}</h1>
                        <ul>
                            {messages.map(function(msg, idx){
                                switch(msg.type) {
                                    case 'join':
                                        return <li key={idx}>{msg.username} JOINED {msg.roomname}</li>;
                                    case 'leave':
                                        return <li key={idx}>{msg.username} LEAVED {msg.roomname}</li>;
                                    case 'msg':
                                        return <li key={idx}>{msg.username}: {msg.payload}</li>;
                                    default:
                                        break
                                }
                            })}
                        </ul>
                    </div>
                    <div className="pure-u-1">
                        <Input type="text" ref="sendBox" />
                        <Button label="Send" onClick={() => this.send()}/>
                    </div>
                </div>
            );
        } else {
            return <div>Hello</div>;
        }
    }

    send() {
        const { dispatch, currentRoom } = this.props;
        const textNode = this.refs.sendBox;
        let text = (textNode.getValue() || '').trim();
        textNode.setValue('');

        if (currentRoom == null) {
            return;
        }
        dispatch(sendMsg(currentRoom, text));
    };
}


ChatRoom.propTypes = {
};


function select(state) {
    return {
        currentRoom: state.chat.currentRoom,
        roomMap: state.chat.roomMap
    };
}

export default connect(select)(ChatRoom);
