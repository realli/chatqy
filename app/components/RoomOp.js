import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { Dialog, Input, Button } from 'react-toolbox';
import { joinInRoom, touchAndJoinRoom } from '../actions/chat';

class RoomOp extends Component {
    constructor() {
        super();
        this.actions_create = [
            {label: "Cancel", onClick: this.closeDialog.bind(this, 'createDialog')},
            {label: "OK", onClick: this.createRoom.bind(this) },
        ];
        this.actions_join = [
            {label: "Cancel", onClick: this.closeDialog.bind(this, 'joinDialog')},
            {label: "OK", onClick: this.joinRoom.bind(this) },
        ];
    }

    showDialog(dialog) {
        this.refs[dialog].show();
    };
    closeDialog(dialog) {
        this.refs[dialog].hide();
    };

    createRoom () {
        let nameNode = this.refs.roomname,
            descNode = this.refs.desc;
        const { dispatch } = this.props;
        let name = nameNode.getValue().trim(),
            desc = descNode.getValue();

        nameNode.setValue('');
        descNode.setValue('');
        dispatch(touchAndJoinRoom(name, desc));

        this.refs.createDialog.hide();
    };

    joinRoom () {
        const { dispatch } = this.props;
        let nameNode = this.refs.joinName;
        let name = nameNode.getValue().trim();
        nameNode.setValue('');
        dispatch(joinInRoom(name));

        this.refs.joinDialog.hide();
    };


    render () {
        return (
            <div>
            <Button label="Create Room" onClick={this.showDialog.bind(this, 'createDialog')} />
            <Button label="Join Room" onClick={this.showDialog.bind(this, 'joinDialog')} />
            <Dialog ref='createDialog' title="Create Room" actions={this.actions_create}>
              <Input type="text" label="Room Name" ref="roomname" />
              <Input type="text" label="description" ref="desc" multiline={true}/>
            </Dialog>
            <Dialog ref='joinDialog' title="Join Room" actions={this.actions_join}>
              <Input type="text" label="Room Name" ref="joinName" />
            </Dialog>
            </div>
        );
    }
}

export default connect()(RoomOp);
