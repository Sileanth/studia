#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

#define LED_DDR DDRD
#define LED_PORT PORTD
#define DELAY_TIME 85


#define BTN1 PB4
#define BTN2 PB1
#define BTN3 PB2
#define BTN_PIN PINB
#define BTN_PORT PORTB

int main() {
  //setuping led port
  /* UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0); */
  /* LED_DDR |= 0xFF; */
  /* LED_PORT = 0xFF; */

  // setuping button port
    BTN_PORT |= _BV(BTN1 | BTN2 | BTN3);


  while(1) {
    if ((BTN_PIN & _BV(BTN1))) {
      printf("xd");
      LED_PORT = 0xFF;
      _delay_ms(1000);
      LED_PORT = 0;
      _delay_ms(1000);
    }
    /* if((BTN_PIN & _BV(BTN2))) { */
    /*   LED_PORT = 0; */
    /*   _delay_ms(50); */
    /* } */
  }


  /* int left = 1; */
  /* while (1) { */
  /*   if(left) { */
  /*     LED_PORT <<= 1; */
  /*     _delay_ms(DELAY_TIME); */
  /*     if(LED_PORT == (1 << 7)) { */
  /*       left = 0; */
  /*     } */
  /*   } */
  /*   else { */
  /*     LED_PORT >>= 1; */
  /*     _delay_ms(DELAY_TIME); */
  /*     if(LED_PORT == 1) { */
  /*       left = 1; */
  /*     } */
  /*   } */
  /* } */
}

