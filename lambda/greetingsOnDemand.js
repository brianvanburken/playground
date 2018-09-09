exports.handler = (event, context, callback) => {
    const name = 'name' in event
        ?  event['name']
        : 'World';
    const greeting = process.env.GREETING
        ? process.env.GREETING
        : 'Hello'
    const greetings = greeting + ' ' + name + '!';
    callback(null, greetings);
};
