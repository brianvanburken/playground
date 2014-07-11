Oreo - simple JavaScript cookies
======

Get, set, or delete cookies.

* [c]2014 @brianvanburken
* Licensed MIT

Based on code examples from https://github.com/filamentgroup/cookie.

# API

To get the value of a cookie only pass the name. It'll return ```null``` if
undefined.
```js
Oreo.get('foo');
```

To set a cookie value, pass a name, string value, and optionally number of days
until the cookie expires.
```js
Oreo.set('foo', 'bar', 5);
```

To delete a cookie, pass a name.
```js
Oreo.destroy('foo');
```
