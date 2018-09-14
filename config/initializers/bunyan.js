const bunyan = require('bunyan');
const package = require('../../package.json');

module.exports = function (config) {
  return bunyan.createLogger({
    name: package.name,
    level: config.logLevel
  });
};
