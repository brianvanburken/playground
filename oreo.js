(function (w, d) {
  "use strict";

  var oreo = {};
  oreo.set = function (name, value, expirationDate) {
    var expires = '; expires=' + expirationDate.toUTCString();
    d.cookie = name + '=' + value + expires + '; path=/';
  };

  oreo.destroy = function (name) {
    oreo.set(name, false, -1);
  };

  oreo.get = function (name) {
    var cookiestring = '; ' + d.cookie;
    var cookies = cookiestring.split('; ' + name + '=');
    if (cookies.length === 2) {
      return cookies.pop().split(';').shift();
    }
    return null;
  };

  w.Oreo = oreo;
})(this, document);
