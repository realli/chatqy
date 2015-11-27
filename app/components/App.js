import React, {Component, PropTypes} from 'react';
import { Link } from 'react-router';
import { connect } from 'react-redux';
import ErrorDialog from './ErrorDialog';
import { user_logout } from '../actions/auth';
import { Navigation, AppBar } from 'react-toolbox';
import { pushState } from 'redux-router';

class App extends Component {
    render () {
        const { dispatch, is_login } = this.props;
        let actions = [];
        if(!is_login) {
            actions.push({label: "Login",
                       onClick: function(){dispatch(pushState({}, '/login'))}},
                       {label: "SignUp",
                           onClick: function(){dispatch(pushState({}, '/signup'))}});
        } else {
            actions.push({label: "Logout",
                       onClick: function(){dispatch(user_logout())}});
        }
        return (
            <div>
                <AppBar flat>
                    <a href="/">Wat</a>
                </AppBar>
                <div className="pure-g">
                    <div className="pure-u-1-5">
                        <Navigation type="vertical" actions={actions} />
                    </div>
                    <div className="pure-u-4-5">
                        <ErrorDialog />
                        {this.props.children} 
                    </div>
                </div>
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
