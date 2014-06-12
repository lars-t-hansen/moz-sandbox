function Array_build(k, fn) {
    var result = new Array(k);
    return Par.invoke(fn, result, [[0,k]]);
}

function Array_map(fn) {
    var self = this;
    var k = self.length;
    var result = new Array(k);
    return Par.invoke(function (i) { return fn(self[i], i, self); }, result, [[0,k]]);
}

function Array_reduce(fn) {

}
