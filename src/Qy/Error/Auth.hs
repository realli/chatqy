module Qy.Error.Auth where

import GHC.Generics
import Servant
import Data.CaseInsensitive (mk)
import Data.Aeson
import Data.Text
import Qy.Error

data AuthErr = AuthErr {
      status :: Int
    , message :: Text
    } deriving (Show, Generic)

instance ToJSON AuthErr
userNotExists :: AuthErr
userNotExists = AuthErr 40101 "User Not Exist"

userAlreadyExists :: AuthErr
userAlreadyExists= AuthErr 40102 "User Already Exist"

passwordInvalid :: AuthErr
passwordInvalid = AuthErr 40103 "Password Invalid"

tokenNotPresent :: AuthErr
tokenNotPresent = AuthErr 40105 "Access Token is Invalid"

tokenInvalid :: AuthErr
tokenInvalid = AuthErr 40106 "Access Token is Invalid"

tokenExpire :: AuthErr
tokenExpire = AuthErr 40107 "Access Token is Expired"

refreshTokenInvalid :: AuthErr
refreshTokenInvalid = AuthErr 40108 "refresh Token is invalid or expired"

userNameIsEmpty :: AuthErr
userNameIsEmpty = AuthErr 40109 "user name is empty"

passwordTooShort :: AuthErr
passwordTooShort = AuthErr 40110 "password is too short, at least 6"


instance ToServantErr AuthErr where
    toServantErr es = err401 { errBody = encode es
                             , errHeaders = [(mk "Content-Type", "application/json")]
                             }

