exports.handler = (event, context, callback) => {
    const name = 'name' in event
        ?  event['name']
        : 'World';
    const greetings = 'Hello ' + name + '!';
    callback(null, greetings);
};
