import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { pushState } from 'redux-router';
import { getRoomList, initConnect, enterRoom } from '../actions/chat';
import RoomOp from './RoomOp';
import ChatRoom from './ChatRoom';
import styles from './ChatRoom.css';
import CSSModules from 'react-css-modules';

@CSSModules(styles)
class ChatRooms extends Component {
    render() {
        const { roomMap } = this.props;

        const lst = [];
        for (let p in roomMap) {
            lst.push({name: p, description: roomMap[p].description})
        }

        return (
            <div styleName="wrap">
                <div styleName="room-list">
                    <RoomOp />
                    <ul>
                        {lst.map(function(room, idx){
                            return (
                                <li styleName="room-list-item" key={idx} onClick={() => this.handleClick(room.name)}>
                                    <h3 styleName="room-item-head">{room.name}</h3>
                                    <p styleName="room-item-content">{room.description}</p>
                                </li>);
                        }, this)}
                    </ul>
                </div>
                <div styleName="main">
                    <ChatRoom />
                </div>
            </div>
        );
    }

    handleClick(roomname) {
        const { dispatch } = this.props;
        dispatch(enterRoom(roomname));
    }

    componentWillMount() {
        const {dispatch} = this.props;
        dispatch(getRoomList());
        dispatch(initConnect());
    }
}


ChatRooms.propTypes = {
    roomMap: PropTypes.object
};


function select(state) {
    return {
        roomMap: state.chat.roomMap
    };
}

export default connect(select)(ChatRooms);
