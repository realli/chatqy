import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { joinInRoom, touchAndJoinRoom } from '../actions/chat';

import styles from './ChatRoom.css';
import CSSModules from 'react-css-modules';

@CSSModules(styles)
class RoomOp extends Component {
    constructor() {
        super();
        this.state = {
            createActivate: false,
            joinActivate: false,
        };
    }

    render () {
        return (
            <div styleName="roomop">
              <button styleName="roomop-button"
                      onClick={() => this.setState({createActivate: true})}
                      >Create Room</button>
              <button styleName="roomop-button"
                      onClick={() => this.setState({joinActivate: true})}
                      >Join Room</button>

              <CreateRoomPopup activate={this.state.createActivate}
                               createHandler={this.createHandler.bind(this)}/>
              <JoinRoomPopup activate={this.state.joinActivate}
                             joinHandler={this.joinHandler.bind(this)}/>
            </div>
        );
    }

    createHandler (roomname, description, canceled) {
        const { dispatch } = this.props;
        this.setState({createActivate: false});
        if (canceled) {
            return;
        }
        dispatch(touchAndJoinRoom(roomname, description));
    }

    joinHandler (roomname, canceled) {
        const { dispatch } = this.props;
        this.setState({joinActivate: false});
        if (canceled) {
            return;
        }
        dispatch(joinInRoom(roomname));
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
                <form styleName="roomop-form">
                <fieldset>
                  <legend>Create A New Room</legend>
                  <label htmlFor="roomname">RoomName</label>
                  <input id="roomname" type="text" placeholder="RoomName" ref="roomname" />

                  <label htmlFor="desc">Description</label>
                  <textarea placeholder="description.."
                            id="desc"
                            ref="desc"
                            ></textarea>

                  <button type="submit"
                          styleName="roomop-form-button"
                          onClick={(e) => this.create(e)}
                          >Create</button>
                  <button styleName="roomop-form-cancel-button"
                          onClick={(e) => this.cancel(e)}
                          >Cancel</button>
                </fieldset>
                </form>
                </div>
                </div>
            );
        } else {
            return;
        }
    }

    create(e) {
        const { createHandler } = this.props;
        e.preventDefault();
        let roomname = this.refs.roomname.value;
        let desc = this.refs.desc.value;
        createHandler(roomname, desc);
    }
    cancel(e) {
        const { createHandler } = this.props;
        e.preventDefault();
        createHandler(undefined, undefined, true);
    }
}

@CSSModules(styles)
class JoinRoomPopup extends Component {
    render () {
        const { activate } = this.props;
        if (activate){
            return (
                <div styleName="black_overlay">
                <div styleName="white_content">
                <form styleName="roomop-form">
                <fieldset>
                  <legend>Create A New Room</legend>
                  <label htmlFor="roomname">RoomName</label>
                  <input id="roomname" type="text" placeholder="RoomName" ref="roomname" />

                  <button styleName="roomop-form-button"
                          onClick={(e) => this.join(e)}
                          >Join</button>
                  <button styleName="roomop-form-cancel-button"
                          onClick={(e) => this.cancel(e)}
                          >Cancel</button>
                </fieldset>
                </form>
                </div>
                </div>
            );
        } else {
            return;
        }
    }

    join(e) {
        const { joinHandler } = this.props;
        e.preventDefault();
        let roomname = this.refs.roomname.value;
        joinHandler(roomname);
    }
    cancel(e) {
        const { joinHandler } = this.props;
        e.preventDefault();
        joinHandler(undefined, true);
    }
}

export default connect()(RoomOp);

