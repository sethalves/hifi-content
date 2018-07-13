/* global libContents:true, module */

function libContents(fuh) {

    var Tool = function () {
    };

    Tool.prototype = {

        // fuh: fuh,

        doSomething: function () {
            // print("QQQQ something: " + this.fuh);
            print("QQQQ something: " + fuh);
        }

    };

    return new Tool();
}


module.exports = {
    libContents: libContents
};
