var publicPath = "http://headache.hungry.com/~seth/hifi/portal/";
// var basePath = "file:///home/seth/src/hifi-content/portal/";
var basePath = "file:///c:/Users/alves/src/hifi-content/portal/";
print("Base path " + basePath);
Resources.overrideUrlPrefix(publicPath, basePath);
