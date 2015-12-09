import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { deleteError } from '../actions';


import styles from './ErrorDialog.css';
import CSSModules from 'react-css-modules';

@CSSModules(styles)
class ErrorDialog extends Component {
    constructor(props) {
        super(props);
        this.state = { hidden: true, timer: null };
    }

    componentWillReceiveProps(nextProps) {
        const { dispatch } = this.props;

        if (nextProps.errorMessages.length !== 0) {
            // show error message
            this.setState({hidden: false});
            if (this.state.timer != null) {
                clearTimeout(this.state.timer);
            }
            this.state.timer = setTimeout(function(){dispatch(deleteError())}, 3000);
        } else if(nextProps.errorMessages.length === 0){
            // hide this dialog
            this.setState({hidden: true});
        }
    }

    componentWillUnmount () {
        if (this.state.timer != null) {
            clearTimeout(this.state.timer);
            this.setState({timer: null});
        }
    }

    render () {
        const { errorMessages } = this.props;
        return (
            <div hidden={this.state.hidden} styleName="wrap">
              <div styleName="dialog">
                {errorMessages.map(
                  (m, i) => <p key={i}>{m}</p>
                )}
              </div>
            </div>
        );
    }
}

ErrorDialog.defaultProps = {
    onDismiss: function () {}
};

ErrorDialog.propTypes = {
    errorMessages: PropTypes.arrayOf(PropTypes.string).isRequired,
    onDismiss: PropTypes.func
};

function select(state) {
    return {
        errorMessages: state.errMsg.messages
    };
}
export default connect(select)(ErrorDialog);
