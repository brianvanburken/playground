module.exports = function (config, { router, logger, database }) {
  
  router.get('/postcodes/', function (request, response) {
    const collection = database.collection('postcodes');
    collection.find({}).toArray(function(error, docs) {
      if (error) {
        response.status(400).json({ error });
      } else {
        response.status(200).json(docs);
      }
    });
  });

  router.post('/postcodes/', function (request, response) {
    const collection = database.collection('postcodes');
    collection.insertOne(request.body, function(error, doc) {
      if (error) {
        response.status(400).json({ error });
      } else {
        response.status(201).json(doc);
      }
    });
  });

}
