//
// Created by lukas on 21.02.2023.
//
#include <stdio.h>

//zad 3
int zero(int x, int k) {
    x
}

int light(int x, int k) {
    return x & (1 << k);
}

//zad 4
int f1(int x, int y) {
    return x << y;
}

int f2(int x, int y) {
    return x >> y;
}

int f3(int x, int y) {
    return x & ((1 << y) - 1) ;
}

int f4(int x, int y) {
    int z = x >> y;

}

// zad 5
#include <stdio.h>

int reverse(int a) {
    int x = 2147483647;
    int y = x + 1;
    int min_one = x + y;
    return a * min_one;
}


//zad 6
int swap8(int a, int b) {
    int c = f3(a, 8) ^ f3(b, 8);
    
}

int main() {

}