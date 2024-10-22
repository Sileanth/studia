#include <avr/io.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <util/delay.h>

// START of IO magic
#define BAUD 9600                              // baudrate
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD) - 1) // zgodnie ze wzorem

// inicjalizacja UART
void uart_init() {
  // ustaw baudrate
  UBRR0 = UBRR_VALUE;
  // wyczyść rejestr UCSR0A
  UCSR0A = 0;
  // włącz odbiornik i nadajnik
  UCSR0B = _BV(RXEN0) | _BV(TXEN0);
  // ustaw format 8n1
  UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

// transmisja jednego znaku
int uart_transmit(char data, FILE *stream) {
  // czekaj aż transmiter gotowy
  while (!(UCSR0A & _BV(UDRE0)))
    ;
  UDR0 = data;
  return 0;
}

// odczyt jednego znaku
int uart_receive(FILE *stream) {
  // czekaj aż znak dostępny
  while (!(UCSR0A & _BV(RXC0)))
    ;
  return UDR0;
}

FILE uart_file;
// END of IO magic

#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BTN PB4
#define BTN_PIN PINB
#define BTN_PORT PORTB

#define PRESSED_BUTTON (!(BTN_PIN & _BV(BTN)))

#define OFF_LED (LED_PORT &= ~_BV(LED))
#define ON_LED (LED_PORT |= _BV(LED))

#define DOT_TRESHOLD 20
#define DASH_TRESHOLD 60
#define CHARACTER_TRESHOLD 60
#define WORD_TRESHOLD 120
#define UNIT_OF_TIME 50

typedef struct {
  uint8_t bitmask;
  uint8_t bit_length;
} morse_code_t;
const morse_code_t empty_morse = {.bitmask = 0b0, .bit_length = 0};

morse_code_t morse_map[] = {
    {0b01, 2},   // A (.-)
    {0b1000, 4}, // B (-...)
    {0b1010, 4}, // C (-.-.)
    {0b100, 3},  // D (-..)
    {0b0, 1},    // E (.)
    {0b0010, 4}, // F (..-.)
    {0b110, 3},  // G (--)
    {0b0000, 4}, // H (....)
    {0b00, 2},   // I (..)
    {0b0111, 4}, // J (.---)
    {0b101, 3},  // K (-.-)
    {0b0100, 4}, // L (.-..)
    {0b11, 2},   // M (--)
    {0b10, 2},   // N (-.)
    {0b111, 3},  // O (---)
    {0b0110, 4}, // P (.--.)
    {0b1101, 4}, // Q (--.-)
    {0b010, 3},  // R (.-.)
    {0b000, 3},  // S (...)
    {0b1, 1},    // T (-)
    {0b001, 3},  // U (..-)
    {0b0001, 4}, // V (...-)
    {0b011, 3},  // W (.--)
    {0b1001, 4}, // X (-..-)
    {0b1011, 4}, // Y (-.--)
    {0b1100, 4}, // Z (--..)
};

char decode_morse(morse_code_t morse_code) {
  char ans = '?';
  for (int8_t i = 0; i < 26; i++) {
    if ((morse_code.bit_length == morse_map[i].bit_length) &&
        (morse_code.bitmask == morse_map[i].bitmask)) {
      ans = 'A' + i;
      if(ans == 'D') {
        printf("DEBUG: %i, %i\n", morse_code.bit_length, morse_code.bitmask);
      }
      break;
    }
  }

  return ans;
}

int8_t button_pressed() {
  OFF_LED;
  int8_t ans = 0;
  int16_t wait_time = 0;
  while (PRESSED_BUTTON) {
    wait_time += 1;

    if (wait_time == DOT_TRESHOLD) {
      ans = 1;
      ON_LED;
    }
    _delay_ms(UNIT_OF_TIME);
  }

  OFF_LED;
  // Add a short delay to avoid false triggers
  _delay_ms(50);

  return ans; // Return 0 for dot, 1 for dash
}

void main_loop() {
  int16_t wait_time = 0;

  morse_code_t mors = empty_morse;
  while (1) {
    _delay_ms(UNIT_OF_TIME);
    wait_time += 1;
    if (PRESSED_BUTTON) {
      int8_t code = button_pressed();
      mors.bitmask |= (code << mors.bit_length);
      mors.bit_length++;
      wait_time = 0;
    }

    if(mors.bit_length > 5) {
      OFF_LED;
      wait_time = 0;
      printf("?");
      mors = empty_morse;
    }
    if (wait_time == CHARACTER_TRESHOLD) {
      ON_LED;
      char c = decode_morse(mors);
      if(mors.bit_length > 0) {
        printf("%c", c);
      }
      mors = empty_morse;
    } 
    if( wait_time >= WORD_TRESHOLD) {
      OFF_LED;
      printf(" ");
      wait_time = 0;
    }
  }
}

int main() {
  // START of IO magic
  // zainicjalizuj UART
  uart_init();
  // skonfiguruj strumienie wejścia/wyjścia
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  // program testowy
  printf("Ready for translating!\r\n");
  // END of IO magic

  // SETUPING PORTS
  BTN_PORT |= _BV(BTN);
  LED_DDR |= _BV(LED);

  /* main_loop(); */
  main_loop();
}
