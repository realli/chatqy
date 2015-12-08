import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { joinInRoom, touchAndJoinRoom } from '../actions/chat';

import styles from './ChatRoom.css';
import CSSModules from 'react-css-modules';

@CSSModules(styles)
class RoomOp extends Component {
    constructor() {
        super();
    }


    render () {
        return (
            <div styleName="roomop">
              <button styleName="roomop-button">Create Room</button>
              <button styleName="roomop-button">Join Room</button>

              <CreateRoomPopup activate={true}/>
            </div>
        );
    }
}

@CSSModules(styles)
class CreateRoomPopup extends Component {
    render () {
        const { activate } = this.props;
        if (activate){
            return (
                <div styleName="black_overlay">
                <div styleName="white_content">
                 Can create Room Here
                </div>
                </div>
            );
        } else {
            return;
        }
    }
}

export default connect()(RoomOp);

