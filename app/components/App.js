import React, {Component, PropTypes} from 'react';
import { Link } from 'react-router';
import { connect } from 'react-redux';
import ErrorDialog from './ErrorDialog';
import { user_logout } from '../actions/auth';
import { pushState } from 'redux-router';
import CSSModules from 'react-css-modules';
import styles from './App.css';

@CSSModules(styles)
class App extends Component {
    render () {
        const { dispatch, is_login } = this.props;
        let menuItems = [];
        if(!is_login) {
            menuItems.push(
                <li styleName="menu-item" key="login">
                  <Link to="/login" styleName="menu-link">Login</Link>
                </li>,
                <li styleName="menu-item" key="signup">
                  <Link to="/signup" styleName="menu-link">SignUp</Link>
                </li>
            );
        } else {
            menuItems.push(
                <li styleName="menu-item" key="logout">
                <Link to="/" styleName="menu-link" onClick={() => dispatch(user_logout())}>
                  Logout
                </Link>
                </li>
            );
        }

        return (
            <div styleName="wrap">
                <div styleName="menu">
                    <Link styleName="menu-heading" to="/">Wat</Link>
                    <ul styleName="menu-list">
                      {menuItems.map(it => it)}
                    </ul>
                </div>
                {this.props.children} 
            </div>
        );
    }
}

App.propTypes = {
    is_login: PropTypes.bool.isRequired,
};

function select(state) {
    return {
        is_login: state.auth.is_login,
    };
}

export default connect(select)(App);
