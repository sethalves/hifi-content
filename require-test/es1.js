/* global Script */

(function() {
    var libFunc = Script.require(Script.resolvePath("lib.js"));
    var lib = libFunc.libContents("1");
    lib.doSomething();
});
