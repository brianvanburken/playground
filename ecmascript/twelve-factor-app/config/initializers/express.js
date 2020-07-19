const express = require('express')
const bodyParser = require('body-parser');

module.exports = function (config, logger) {
  const app = express();

  app.use(bodyParser.urlencoded({ extended: true }));
  app.use(bodyParser.json());
  app.listen(config.port, () => logger.info(`Started on port ${config.port}`));

  const router = express.Router();
  app.use('/api', router);

  return router;
}
