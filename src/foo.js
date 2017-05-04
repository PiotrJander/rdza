function foo() {
    var x = 1;
    var bar = function() {
        x = x + 1;
        console.log("bar", x)
    }
    console.log("foo", x);
    return bar;
    // var z = bar();
    // process.stdout.write(x);
}

var baz = foo();
baz();
baz();
foo();
