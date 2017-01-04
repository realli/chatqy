# ChatQY #

A simple Chat Server build with Haskell servant, Websockets and React.js

Trying to learn web dev by building it.

## TO-DO

Everything

## config-file-example (yaml):
```
chatqy:
    port: 8081
    connStr: "host=localhost dbname=qydb user=dbuser password=123456 port=5432"
    poolNum: 1
    env: "development"
```

`chatqy.yaml` will be parsed when the server start.

## build front-end stuff

### install nodejs and front-end dependency
```
>> npm install
```
### build
```
>> ./node_modules/.bin/webpack
```

## run server
```
stack exec chatqy
```
