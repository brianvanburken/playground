(function (w, d) {
  "use strict";

  var oreo = {};
  oreo.set = function (name, value, days) {
    var expires = '';
    if (days) {
      var date = new Date();
      date.setTime( date.getTime() + (days * 24 * 60 * 60 * 1000) );
      expires = '; expires=' + date.toGMTString();
    }
    d.cookie = name + '=' + value + expires + '; path=/';
  };

  oreo.delete = function (name) {
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
