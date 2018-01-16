const fs = require('fs');
const path = require('path');

module.exports = function (config, modules) {

  const dir = path.join(path.dirname(fs.realpathSync(__filename)), '../app/routes/');

  fs.readdirSync(dir)
    .filter(file => file.endsWith('.js'))
    .map(file => dir + file)
    .forEach(file => {
      require(file)(config, modules);
    });
}
