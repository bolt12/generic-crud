## POST /user

### Request:

- Supported content types are:

- `application/json;charset=utf-8`
- `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"_userId":1,"_userFirstName":"Haskell","_userLastName":"Curry"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

- `application/json;charset=utf-8`
- `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"_userId":1,"_userFirstName":"Haskell","_userLastName":"Curry"}
```

## DELETE /user/:id

### Captures:

- *id*: (PrimaryKey UserT Identity) unique identifier

### Response:

- Status code 200
- Headers: []

- Supported content types are:

- `application/json;charset=utf-8`
- `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"_userId":1,"_userFirstName":"Haskell","_userLastName":"Curry"}
```

## GET /user/:id

### Captures:

- *id*: (PrimaryKey UserT Identity) unique identifier

### Response:

- Status code 200
- Headers: []

- Supported content types are:

- `application/json;charset=utf-8`
- `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"_userId":1,"_userFirstName":"Haskell","_userLastName":"Curry"}
```

## PUT /user/:id

### Captures:

- *id*: (PrimaryKey UserT Identity) unique identifier

### Request:

- Supported content types are:

- `application/json;charset=utf-8`
- `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"_userId":1,"_userFirstName":"Haskell","_userLastName":"Curry"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

- `application/json;charset=utf-8`
- `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"_userId":1,"_userFirstName":"Haskell","_userLastName":"Curry"}
```

## GET /users

### Response:

- Status code 200
- Headers: []

- Supported content types are:

- `application/json;charset=utf-8`
- `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"_userId":1,"_userFirstName":"Haskell","_userLastName":"Curry"}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"_userId":1,"_userFirstName":"Haskell","_userLastName":"Curry"},{"_userId":1,"_userFirstName":"Haskell","_userLastName":"Curry"}]
```
