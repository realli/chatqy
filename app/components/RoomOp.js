import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { joinInRoom, touchAndJoinRoom } from '../actions/chat';

class RoomOp extends Component {
    constructor() {
        super();
    }


    render () {
        return (
            <div>
            TODO
            </div>
        );
    }
}

export default connect()(RoomOp);
