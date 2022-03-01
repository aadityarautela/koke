#include "kbinput.h"
#include "../cpu/cpu.h"
#include "interface.h"

#include <ncurses.h>
#include <stdint.h>

uint8_t QUIT = 0;

void kbinput_listen() {
  char c = getch();
  switch (c) {
  case '\n':
    cpu_exec();
    break;
  case 'r':
    cpu_reset();
    break;
  case 'q':
    QUIT = 1;
    break;
  default:
    break;
  }
}

uint8_t kbinput_should_quit() { return QUIT; }