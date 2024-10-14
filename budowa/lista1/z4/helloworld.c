#include <avr/io.h>
#include <stdint.h>
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



void intt16() {
    // MARK1
    int16_t a;
    int16_t b;
    scanf("%"SCNd16, &a);
    scanf("%"SCNd16, &b);
    int16_t wyn = a + b;
    printf("a+b = %"PRId16"\r\n", wyn);
}


void intt8() {
    // MARK2
    int8_t a;
    int8_t b;
    scanf("%"SCNd8, &a);
    scanf("%"SCNd8, &b);
    int8_t wyn = a + b;
    printf("a+b = %"PRId8"\r\n", wyn);
}


void intt32() {
    // MARK3
    int32_t a;
    int32_t b;
    scanf("%"SCNd32, &a);
    scanf("%"SCNd32, &b);
    int32_t wyn = a + b;
    printf("a+b = %"PRId32"\r\n", wyn);
}


void intt64() {
    // MARK4
    int64_t a;
    int64_t b;
    scanf("%"SCNd32, &a);
    scanf("%"SCNd32, &b);
    int64_t wyn = a + b;
    printf("a+b = %"PRId32"\r\n", wyn);
}

void floatt() {
  // MARK5
  float a;
  float b;
  scanf("%f", &a);
  scanf("%f", &b);
  float wyn = a + b;
  printf("a+b = %f\r\n", wyn);

}


int main()
{
  // zainicjalizuj UART
  uart_init();
  // skonfiguruj strumienie wejścia/wyjścia
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  // program testowy
  printf("Loaded!\r\n");
  intt8();
  while(1) {

  }
}


