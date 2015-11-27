import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { pushState } from 'redux-router';
import {user_login} from '../actions/auth';
import { Input, Button } from 'react-toolbox';


class UserLogin extends Component {
    render() {
        const {dispatch, is_login, is_pending, token, user} = this.props;
        return (
            <div className="pure-g">
            <div className="pure-u">
             <form>
               <fieldset>
                 <legend>
                   <p>Login Please</p>
                 </legend>
                 <Input type="text" label="UserName" icon="account-box" ref="username" required />
                 <Input type="password" label="PassWord" icon="security" ref="password" required />
                 <Button label="Login" type="submit"
                         onClick={e => this.handleClick(e)}
                         disabled={is_pending}/>
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
