const cowsay = require("cowsay");

exports.handler = (event, context, callback) => {
    const name = 'name' in event
        ?  event['name']
        : 'World';
    const greeting = process.env.GREETING
        ? process.env.GREETING
        : 'Hello'
    const text = greeting + ' ' + name + '!';
    const say = cowsay.say({ text });
    callback(null, say);
};
