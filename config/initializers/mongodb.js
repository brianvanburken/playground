const MongoClient = require('mongodb').MongoClient;

module.exports = function (config, logger) {
  return new Promise((resolve, reject) => {
    MongoClient.connect(config.mongoDbUrl, function(error, client) {
      if (error) {
        reject(error);
      } else {
        logger.info(`Connected to mongodb client: ${config.mongoDbUrl}`);
        const database = client.db(config.mongoDbDatabase);
        resolve(database);
      }
    });
  });
}
