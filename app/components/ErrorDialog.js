import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { deleteError } from '../actions';
import { Dialog } from 'react-toolbox';

class ErrorDialog extends Component {
    constructor() {
        super();
        this.actions = [{label: "Ok", onClick: this.dismissError.bind(this)}];
    }

    render () {
        const { errorMessages } = this.props;
        return (
            <Dialog ref="dialogError" title="Error Happened"
                actions={this.actions}>
                {errorMessages.map((m, idx) => {
                    return <p key={idx}>{m}</p>
                })}
            </Dialog>
        );
    }

    shouldComponentUpdate(nextProps) {
        const { errorMessages } = nextProps;
        const show = (errorMessages.length > 0);
        const dia = this.refs.dialogError;
        if(show) {
            dia.show();
        } else {
            dia.hide();
        }
        return true;
    }

    dismissError() {
        const { dispatch } = this.props;
        const dia = this.refs.dialogError;
        dia.hide();
        dispatch(deleteError());
    }
}

ErrorDialog.propTypes = {
    errorMessages: PropTypes.arrayOf(PropTypes.string).isRequired,
};

function select(state) {
    return {
        errorMessages: state.errMsg.messages
    };
}
export default connect(select)(ErrorDialog);
