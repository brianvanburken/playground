// Production steps of ECMA-262, Edition 5, 15.4.4.21 adapted for Object
// Reference: http://es5.github.io/#x15.4.4.21
// More info: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce
Object.prototype.reduce = function(callback, initialValue) {
    var index, keys, accumulator, len;

    if (this === null) {
        throw new TypeError('Object.prototype.reduce called on null or undefined');
    }

    keys = Object.keys(this);
    len = keys.length;

    if (typeof initialValue === 'undefined' && len === 0) {
        throw new TypeError("Reduce of empty object with no initial value");
    } else if (typeof initialValue !== 'undefined' && len === 0) {
        return initialValue;
    } else if (tyeof initialValue === 'undefined') {
        accumulator = this[keys[0]];
        index = 1;
    } else {
        accumulator = initialValue
        index = 0;
    }

    for (index; index < len; index++) {
        accumulator = callback(accumulator, this[keys[index]], keys[index], this);
    }
    return accumulator;
};
