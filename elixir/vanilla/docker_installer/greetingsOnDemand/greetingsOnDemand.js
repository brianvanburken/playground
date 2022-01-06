exports.handler = (event, context, callback) => {
    const name = 'name' in event
        ?  event['name']
        : 'World';
    const greeting = process.env.GREETING
        ? process.env.GREETING
        : 'Hello'
    const say = greeting + ' ' + name + '!';
    callback(null, say);
};
