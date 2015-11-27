import React from 'react';
import { combineReducers } from 'redux';

import authR from './auth';
import errMsgR from './errormessage.js'
import chatR from './chat.js'


export default { authR, errMsgR, chatR};
