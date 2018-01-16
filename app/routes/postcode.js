module.exports = function (config, { router, logger }) {

  router.get('/', function (request, response) {
    logger.info('Got a request!');
    response.json({ ok: true });
  });
}
