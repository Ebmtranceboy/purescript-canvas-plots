exports.getContext2D = function(c) {
    return function() {
        return c.getContext('2d');
    };
};
