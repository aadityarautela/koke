#pragma once

#include <stdint.h>
extern uint8_t DEBUG;

struct Instruction {
  char *name;
  uint8_t (*op)(void);
  uint8_t (*mode)(void);
  uint8_t cycles;
};

void instruction_exec(uint8_t opcode, uint32_t *cycles);
void reset(void);
