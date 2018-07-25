/* global libContents:true, module */

function libContents(fuh) {

    var Tool = function () { };

    Tool.prototype = {
        doSomething: function () {
            print("QQQQ something: " + fuh);
        }
    };

    return new Tool();
}


module.exports = {
    libContents: libContents
};
