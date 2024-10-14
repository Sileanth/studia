#include <avr/io.h>
#include <util/delay.h>

#define LED_DDR DDRD
#define LED_PORT PORTD
#define DELAY_TIME 85

int main() {
  UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
  


  LED_DDR |= 0xFF;
  LED_PORT = 1;
  int left = 1;
  while (1) {
    if(left) {
      LED_PORT <<= 1;
      _delay_ms(DELAY_TIME);
      if(LED_PORT == (1 << 7)) {
        left = 0;
      }
    }
    else {
      LED_PORT >>= 1;
      _delay_ms(DELAY_TIME);
      if(LED_PORT == 1) {
        left = 1;
      }
    }
  }
}

