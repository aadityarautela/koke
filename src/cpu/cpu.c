#include "cpu.h"
#include "../memory/mem.h"
#include "../utils/misc.h"
#include "instruction.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// Little-endian, addr stored in memory LSB first
struct CentralProcessingUnit cpu;

// clock cycles
uint32_t cycles = 0;

// memory reference
struct mem *mem_ptr = NULL;

// cpu initialization
void cpu_init(void) { mem_ptr = mem_get_ptr(); }

uint8_t cpu_extract_sr(uint8_t flag) { return ((cpu.sr >> (flag % 8)) & 1); }

uint8_t cpu_mod_sr(uint8_t flag, uint8_t val) {
  if (val != 0 && val != 1)
    return 1;

  if (flag > 0 && flag < 8 && flag != 5) {
    if (val == 1) {
      SET_BIT(cpu.sr, flag);
    } else {
      CLEAR_BIT(cpu.sr, flag);
    }
    return 0;
  } else {
    return 1;
  }
}

void cpu_reset() {
  reset();
  cycles = 8;
}

static int8_t get_mem(uint16_t addr) {
  debug_print("(get_mem) currently reading at: 0x%X\n", addr);
  if (addr <= 0x00FF)
    return mem_ptr->zero_page[addr];
  else if (addr >= 0x0100 && addr <= 0x01FF)
    return mem_ptr->stack[addr - 0x0100];
  else if (addr >= 0xFFFA)
    return mem_ptr->last_six[addr - 0xFFFA];
  else {
    debug_print("(get_mem) parsed at: 0x%X\n", addr - 0x0200);
    return mem_ptr->data[addr - 0x0200];
  }
}

static uint8_t write_mem(uint16_t addr, uint8_t data) {
  if (addr <= 0x00FF) {
    mem_ptr->zero_page[addr] = data;
  } else if (addr >= 0x0100 && addr <= 0x01FF) {
    mem_ptr->stack[addr - 0x0100] = data;
  } else if (addr >= 0xFFFA) {
    mem_ptr->last_six[addr - 0xFFFA] = data;
  } else {
    mem_ptr->data[addr - 0x0200] = data;
  }
  return 0;
}

uint8_t cpu_fetch(uint16_t addr) {
  debug_print("(cpu_fetch) Reading address: 0x%X\n", addr);
  uint8_t data = get_mem(addr);
  debug_print("(cpu_fetch) Data: 0x%X\n", data);
  if (addr == cpu.pc)
    cpu.pc++;
  return data;
}

uint8_t cpu_write(uint16_t addr, uint8_t data) {
  return write_mem(addr, data) == 1 ? 1 : 0;
}

// Fetch Decode Execute
void cpu_exec() {
  debug_print("(cpu_exec) Cycles: %d, Memory: %p\n", cycles, (void *)mem_ptr);

  int8_t fetched;
  do {
    debug_print("(fde loop) Cycles: %d\n", cycles);
    // exec
    if (cycles == 0) {
      fetched = cpu_fetch(cpu.pc);
      debug_print("(cpu_exec) Fetched: 0x%X\n", fetched);
      instruction_exec(fetched, &cycles);
    }
    cycles--;
  } while (cycles != 0);
}