module.exports = function (config) {
  const logger = require('./initializers/logger')(config);
  const router = require('./initializers/router')(config);

  return {
    logger,
    router
  };
}
