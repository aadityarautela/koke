#include "cpu/cpu.h"
#include "interface/interface.h"
#include "interface/kbinput.h"
#include "memory/mem.h"

#include <ncurses.h>
#include <stdio.h>
#include <stdlib.h>

uint8_t DEBUG = 0;

int main(int argc, char **argv) {
  switch (argc) {
  case 1:
    fprintf(stderr, "[ERROR] Not enough arguments.\n");
    exit(1);

  case 2:
    mem_init(argv[1]);
    break;

  default:
    fprintf(stderr, "[ERROR] Too many arguments.\n");
    exit(1);
  }

  cpu_init();
  cpu_reset();
  WINDOW *window = newwin(WINDOW_ROWS, WINDOW_COLS, 0, 0);
  if ((window = initscr()) == NULL) {
    fprintf(stderr, "[ERROR] Unable to init ncurses.\n");
    exit(1);
  }

  curs_set(0);
  noecho();
  box(window, 0, 0);
  wrefresh(window);

  interface_display_header();
  wrefresh(window);

  do {
    interface_display_cpu();
    interface_display_mem();
    wrefresh(window);
    kbinput_listen();
  } while (!kbinput_should_quit());

  delwin(window);
  endwin();
  mem_dump();
  return 0;
}