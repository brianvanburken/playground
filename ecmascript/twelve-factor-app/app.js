const env = process.env.NODE_ENV || 'development';
const config = require(`./config/environment/${env}.js`);
const initializer = require('./config/initializers.js');
initializer(config)
  .then(modules => require('./config/bootstrap')(config, modules))
  .catch(console.log);
