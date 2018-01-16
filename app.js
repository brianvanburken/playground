const env = process.env.NODE_ENV || 'development';
const config = require(`./config/environment/${env}.js`);
const modules = require('./config/initializers.js')(config);

require('./config/bootstrap')(config, modules);
