import React, {Component, PropTypes} from 'react';
import { Link } from 'react-router';
import { connect } from 'react-redux';
import ChatRooms from './ChatRooms';
import styles from './Home.css';
import CSSModules from 'react-css-modules';


@CSSModules(styles)
class Home extends Component {
    render () {
        const {is_login, username} = this.props;
        if(is_login) {
            return (
                <ChatRooms />
            );
        } else {
            return (
                <div styleName="home-main">
                  <h1>A Very Simple Chat Server</h1>
                  <p>This is a chat room web app written using haskell servant, websockets and react.js.</p>
                  <p>Sign up -> Join some room and chat</p>
                </div>
            );
        }
    }
}

Home.propTypes = {
    is_login: PropTypes.bool.isRequired,
    username: PropTypes.string.isRequired
};

function select(state) {
    return {
        is_login: state.auth.is_login,
        username: state.auth.user.name
    };
}

export default connect(select)(Home);
