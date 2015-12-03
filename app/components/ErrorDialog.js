import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { deleteError } from '../actions';

class ErrorDialog extends Component {
    constructor() {
        super();
    }

    render () {
        const { errorMessages } = this.props;
        return (
            <div>TODO</div>
        );
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
