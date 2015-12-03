import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import {user_signup} from '../actions/auth';

class UserSignup extends Component {
    render() {
        const {dispatch, is_login, is_pending} = this.props;
        return (
            <div className="pure-g">
            <div className="pure-u">
             <form>
               <fieldset>
                 <legend>
                   <p>Sign Up with your email</p>
                 </legend>

               </fieldset>
             </form>
            </div>
            </div>
        );
    }

    componentWillMount() {
        const {dispatch, is_login} = this.props;
        if (is_login) {
            dispatch(pushState({}, '/'));
        }
    }

    handleClick(e) {
        e.preventDefault();
        const name_node = this.refs.username;
        const pass_node = this.refs.password;
        const email_node = this.refs.email;

        const username = (name_node.getValue() || '').trim();
        const password = (pass_node.getValue() || '').trim();
        const email = (email_node.getValue() || '').trim();

        const { dispatch } = this.props;
        dispatch(user_signup(username, password, email));
    }
}

UserSignup.propTypes = {
    is_login: PropTypes.bool.isRequired,
    is_pending: PropTypes.bool.isRequired
};

function select(state) {
    return {
        is_pending: state.auth.signup_pending,
        is_login: state.auth.is_login
    };
}

export default connect(select)(UserSignup);
