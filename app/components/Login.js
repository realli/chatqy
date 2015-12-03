import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { pushState } from 'redux-router';
import {user_login} from '../actions/auth';
import styles from './Form.css';
import CSSModules from 'react-css-modules';

@CSSModules(styles)
class UserLogin extends Component {
    render() {
        const {dispatch, is_login, is_pending, token, user} = this.props;
        return (
             <form styleName="form">
               <fieldset>
                 <legend>Login</legend>
                 <div styleName="field">
                   <label htmlFor="username">Username</label>
                   <input id="username" type="text" ref="username" styleName="input" />
                 </div>
                 <div styleName="field">
                   <label htmlFor="password">Password</label>
                   <input id="password" type="password" ref="password" styleName="input" />
                 </div>

                 <button type="submit"
                         styleName="formButton"
                         disabled={is_pending}
                         onClick={e => this.handleClick(e)}>Login</button>
               </fieldset>
             </form>
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
        const nameNode = this.refs.username;
        const passNode = this.refs.password;
        const username = (nameNode.getValue() || '').trim();
        const password = (passNode.getValue() || '').trim();

        const { dispatch } = this.props;

        dispatch(user_login(username, password));
    }
}


UserLogin.propTypes = {
    is_login: PropTypes.bool.isRequired,
    is_pending: PropTypes.bool.isRequired,
    token: PropTypes.string.isRequired,
    user: PropTypes.shape({
        name: PropTypes.string.isRequired
    }),
};


function select(state) {
    return {
        is_login: state.auth.is_login,
        is_pending: state.auth.login_pending,
        token: state.auth.access_token,
        user: state.auth.user,
    };
}

export default connect(select)(UserLogin);
