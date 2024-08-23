

function fib_rec(n) {
    if (n <= 1) {
        return n;
    }
    return fib_rec(n - 1) + fib_rec(n - 2);
}

function fib_iter(n) {
    if (n <= 1) {
        return n;
    }
    let a = 0;
    let b = 1;
    let c;
    for (let i = 2; i <= n; i++) {
        c = a + b;
        a = b;
        b = c;
    }
    return b;
}


for (let i = 0; i < 100; i++) {
    
    console.time("fib_rec");
    fib_rec(i);
    console.timeEnd("fib_rec");

    console.time("fib_iter");
    fib_iter(i);
    console.timeEnd("fib_iter");
}