#pragma once

#include <stddef.h>
#include <stdint.h>

#define TOTAL_MEM 1024 * 64

/*
    Memory Specifications
    256 Bytes 0x0000 to 0x00FF => Zero Page
    256 Bytes 0x0100 to 0x01FF => System Stack
    0x10000 - 0x206 Bytes      => Program Data
    Last 6 Bytes RESERVED
*/
struct mem {
  uint8_t zero_page[0x100];
  uint8_t stack[0x100];
  uint8_t last_six[0x06];
  uint8_t data[TOTAL_MEM - 0x206];
};

void mem_init(char *filename);
int mem_dump(void);
struct mem *mem_get_ptr(void);