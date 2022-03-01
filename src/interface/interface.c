#include "interface.h"
#include "../cpu/cpu.h"
#include "../memory/mem.h"

#include "ncurses.h"
#include "stdint.h"
#include "stdio.h"

void interface_display_header() {
  mvprintw(1, 4, "Koke emulator");
  mvprintw(2, 4, "----------------------");
  mvprintw(3, 8, "Enter: Execute Next Inst");
  mvprintw(4, 8, "r: Reset");
  mvprintw(5, 8, "q: Quit");
  mvprintw(6, 4, "----------------------");
}

void interface_display_cpu() {
  mvprintw(10, 4, "ac: 0x%X pc: 0x%X sp: 0x%X x: 0x%X y: 0x%X sr: 0x%X", cpu.ac,
           cpu.pc, cpu.sp, cpu.x, cpu.y, cpu.sr);
}

void interface_display_mem(void) {
  struct mem *mp = mem_get_ptr();
  mvprintw(12, 4, "Zero Page:");
  uint8_t x = 4;
  uint8_t y = 14;
  for (uint16_t i = 0; i < 256; i++) {
    mvprintw(y, x, "%02X ", mp->zero_page[i]);
    if (x % WINDOW_ROWS == 0) {
      y += 1;
      x = 3;
    } else {
      x += 3;
    }
  }
  y += 2;
  mvprintw(y, 3, "Stack:");
  y += 2;
  x = 3;
  for (uint16_t i = 0; i < 256; i++) {
    mvprintw(y, x, "%02X ", mp->stack[i]);
    if (x % WINDOW_ROWS == 0) {
      y += 1;
      x = 3;
    } else {
      x += 3;
    }
  }
  y += 2;
  mvprintw(y, 3, "Program Data:");
  y += 2;
  x = 3;
  for (uint16_t i = 0x8000 - 0x0200; i < 0x8000 - 0x0200 + 256; i++) {
    mvprintw(y, x, "%02X ", mp->data[i]);
    if (x % WINDOW_ROWS == 0) {
      y += 1;
      x = 3;
    } else {
      x += 3;
    }
  }
  mvprintw(y + 2, 3, "[...]");
}