/* global Script */

(function() {
    var libAFunc = Script.require(Script.resolvePath("lib.js"));
    var libBFunc = Script.require(Script.resolvePath("lib.js"));

    print("QQQQ HERE 2");

    var libA = libAFunc.libContents("a");
    var libB = libBFunc.libContents("b");

    libA.doSomething();
    libB.doSomething();
})();
