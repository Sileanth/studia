#include <avr/io.h>
#include <stdint.h>
#include <util/delay.h>

#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BTN PB4
#define BTN_PIN PINB
#define BTN_PORT PORTB


// CYCLE BUFFER
#define E_EMPTY 0
#define E_ONE 1
#define E_ZERO 2
#define CYCLE_BUFFER_SIZE 100
int8_t cycle_buffer[CYCLE_BUFFER_SIZE];
int8_t cur_idx = 0;

void incr_index() {
  cur_idx += 1;
  if(cur_idx >= CYCLE_BUFFER_SIZE) {
    cur_idx -= CYCLE_BUFFER_SIZE;
  }
}
int8_t read_value() {
  return cycle_buffer[cur_idx];
}

void set_value(int8_t new_value) {
  cycle_buffer[cur_idx] = new_value; 
}

// BUTTON STATE
int8_t previous_state = 0; // 0 for not pressed, 1 for pressed



int main() {
  BTN_PORT |= _BV(BTN);
  LED_DDR |= _BV(LED);
  while (1) {

    // replaying history
    int8_t old_val = read_value();
    if(old_val == E_ONE) {
      LED_PORT &= ~_BV(LED);
    } else {
      LED_PORT |= _BV(LED);
    }
    

    // capturing current state
    if (BTN_PIN & _BV(BTN)) {
      set_value(E_ONE);
    }
    else {
      set_value(E_ZERO);
    }


    incr_index();
    // sleep
    _delay_ms(10);


  }
}

