const express = require('express')
const bodyParser = require('body-parser');

module.exports = function (config) {
  const app = express();

  app.use(bodyParser.urlencoded({ extended: true }));
  app.use(bodyParser.json());
  app.listen(config.port);

  const router = express.Router();
  app.use('/api', router);

  return router;
}
