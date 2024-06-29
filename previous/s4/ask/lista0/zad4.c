
#include <stdio.h>
#include <stdbool.h>
#define ui uint32_t
//zad 3
ui zero(ui x, ui k) {
    return x - ((1 << k) & x);
}

ui light(ui x, ui k) {
    return x | (1 << k);
}

ui switch(ui x, ui k) {
    return x ^ (1 << k);
}

//zad 4
ui f1(ui x, ui y) {
    return x << y;
}

ui f2(ui x, ui y) {
    return x >> y;
}

ui f3(ui x, ui y) {
    return x & ((1 << y) - 1) ;
}

ui f4(ui x, ui y) {
    
}

// zad 5
int minus(int x)
{
    return ~x + 1;
}


//zad 6
void swap8(ui a, ui b) {
    ui c = a & 255;
    a = f1(f2(a, 8),8) | (b & 255);
    b = f1(f2(b, 8, 8) | c;   
}
//zad 7
ui pot(ui x) {
    return ((x-1) & x) | ((x-1) & (1 << 31));
}

// zad 8

ui convert(ui x) {
    int s1 = (255 & x) << 24;
    int s2 = s1 | (((255 << 8) & x) << 8);
    int s3 = s2 | (((255 << 16) & x) >> 8);
    int s4 = s3 | (((255 << 24) & x) >> 24);
    return s4;
}

ui main() {

}