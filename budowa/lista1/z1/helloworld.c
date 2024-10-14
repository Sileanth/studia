#include <avr/io.h>
#include <util/delay.h>
#include <ctype.h>
#include <stdio.h>
#include <inttypes.h>

#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

// inicjalizacja UART
void uart_init()
{
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
int uart_transmit(char data, FILE *stream)
{
  // czekaj aż transmiter gotowy
  while(!(UCSR0A & _BV(UDRE0)));
  UDR0 = data;
  return 0;
}

// odczyt jednego znaku
int uart_receive(FILE *stream)
{
  // czekaj aż znak dostępny
  while (!(UCSR0A & _BV(RXC0)));
  return UDR0;
}

FILE uart_file;



   const char* morse_table[26] = {
        ".-",   
        "-...", 
        "-.-.", 
        "-..",  
        ".",    
        "..-.", 
        "--.",  
        "....", 
        "..",   
        ".---", 
        "-.-",  
        ".-..", 
        "--",   
        "-.",   
        "---",  
        ".--.", 
        "--.-", 
        ".-.",  
        "...",  
        "-",    
        "..-",  
        "...-", 
        ".--",  
        "-..-", 
        "-.--", 
        "--..", 
    };


#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

int main()
{
  // zainicjalizuj UART
  uart_init();
  // skonfiguruj strumienie wejścia/wyjścia
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  
  printf("Ready for translating!\n");

  // ustaw direction pinów
  LED_DDR |= _BV(LED);
    
 

  while(1) {
    char c = getchar();
    c = tolower(c);

    if( c > 'z' || c < 'a') {
      continue;
    }
    char* encoding = morse_table[c - 'a'];


    while(*encoding != '\0') {
      char z = *encoding;
      if(z == '.') {
        printf(".");
        LED_PORT |= _BV(LED);
      _delay_ms(100);
      } else {
        printf("-");
        LED_PORT |= _BV(LED);
      _delay_ms(1000);
      }
      encoding++;
      LED_PORT &= ~_BV(LED);
      _delay_ms(500);
    }
      
      printf("\n");
      _delay_ms(1000);



  }
}


