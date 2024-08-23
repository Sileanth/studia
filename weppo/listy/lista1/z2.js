

function digits(number) {
    let digits = [];
    while (number > 0) {
        digits.push(number % 10);
        number = Math.floor(number / 10);
    }
    return digits;
}

function forall(predicate, arr) {
    for (let i = 0; i < arr.length; i++) {
        if (!predicate(arr[i])) {
            return false;
        }
    }
    return true;
}




function fajne_liczby(number) {
    let digitsArr = digits(number);
    let sum = digitsArr.reduce((acc, x) => acc + x, 0);
    return forall((x) => number % x === 0, digitsArr) &&  number % sum === 0;
}



for (let i = 1; i <= 1000000; i++) {
    if (fajne_liczby(i)) {
        console.log(i);
    }
}