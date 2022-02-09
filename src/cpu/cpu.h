#pragma once
#include <stdint.h>
struct CentralProcessingUnit {
  uint16_t pc;
  uint8_t sp;
  uint8_t ac;
  uint8_t x;
  uint8_t y;
  uint8_t sr; // status register
};

#define C 0 // carry
#define Z 1 // zero
#define I 2 // interrupt
#define D 3 // decimal
#define B 4 // break
#define V 6 // oVerflow
#define N 7 // negative

extern struct CentralProcessingUnit cpu;

void cpu_reset(void);
uint8_t cpu_extract_sr(uint8_t flag);
uint8_t cpu_mod_sr(uint8_t flag, uint8_t val);
uint8_t cpu_fetch(uint16_t addr);
uint8_t cpu_write(uint16_t addr, uint8_t data);
void cpu_exec();
void cpu_init(void);
