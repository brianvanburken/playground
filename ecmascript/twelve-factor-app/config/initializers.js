module.exports = async function (config) {
  const path = './initializers/';

  const logger = require(`${path}bunyan`)(config);
  const router = require(`${path}express`)(config, logger);
  const database = await require(`${path}mongodb`)(config, logger);

  return { logger, router, database };
}
