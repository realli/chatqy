import React from 'react';
import { render } from 'react-dom';
import { createStore, applyMiddleware, compose, combineReducers } from 'redux';
import { Provider, connect } from 'react-redux';
import thunkMiddleware from 'redux-thunk';
import createLogger from 'redux-logger';

import { Route, IndexRoute, Link } from 'react-router';
import { ReduxRouter,
         routerStateReducer,
         reduxReactRouter
} from 'redux-router';


import UserLogin from './components/Login';
import UserSignup from './components/Signup';
import {authR, errMsgR, chatR} from './reducers';
import App from './components/App';
import Home from './components/Home';

import createHistory from 'history/lib/createBrowserHistory';
import {persistStore, autoRehydrate} from 'redux-persist';

let NApp = connect(state => ({routerState: state.router}))(App);

const loggerMiddleware = createLogger();

const reducer = combineReducers({
    router: routerStateReducer,
    auth: authR,
    errMsg: errMsgR,
    chat: chatR
});

let store = compose(
    autoRehydrate(),
    applyMiddleware(thunkMiddleware, loggerMiddleware),
    reduxReactRouter({createHistory}),
)(createStore)(reducer);

persistStore(store, {blacklist: ['router', 'chat', 'errMsg', 'socketMan']});


let rootElement = document.createElement('app');
document.body.appendChild(rootElement);
render (
    <Provider store={store}>
      <ReduxRouter>
        <Route path="/" component={NApp}>
          <IndexRoute component={Home} />
          <Route path="login" component={UserLogin} />
          <Route path="signup" component={UserSignup} />
        </Route>
      </ReduxRouter>
    </Provider>, rootElement);
