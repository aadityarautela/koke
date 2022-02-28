#include "instruction.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "../utils/misc.h"
#include "cpu.h"

/*
    Instruction Modes Prototypes
*/

static uint8_t IMP(void);
static uint8_t IMM(void);
static uint8_t ZP0(void);
static uint8_t ZPX(void);
static uint8_t ZPY(void);
static uint8_t ABS(void);
static uint8_t ABX(void);
static uint8_t ABY(void);
static uint8_t IND(void);
static uint8_t IZX(void);
static uint8_t IZY(void);
static uint8_t REL(void);

/*
    Operations Prototypes
*/

static uint8_t XXX(void);
static uint8_t LDA(void);
static uint8_t LDX(void);
static uint8_t LDY(void);
static uint8_t BRK(void);
static uint8_t BPL(void);
static uint8_t JSR(void);
static uint8_t BMI(void);
static uint8_t RTI(void);
static uint8_t BVC(void);
static uint8_t RTS(void);
static uint8_t BVS(void);
static uint8_t NOP(void);
static uint8_t BCC(void);
static uint8_t BCS(void);
static uint8_t BNE(void);
static uint8_t CPX(void);
static uint8_t CPY(void);
static uint8_t BEQ(void);
static uint8_t ORA(void);
static uint8_t AND(void);
static uint8_t EOR(void);
static uint8_t BIT(void);
static uint8_t ADC(void);
static uint8_t STA(void);
static uint8_t STX(void);
static uint8_t STY(void);
static uint8_t CMP(void);
static uint8_t SBC(void);
static uint8_t ASL(void);
static uint8_t ROL(void);
static uint8_t LSR(void);
static uint8_t ROR(void);
static uint8_t DEC(void);
static uint8_t DEX(void);
static uint8_t DEY(void);
static uint8_t INC(void);
static uint8_t INX(void);
static uint8_t INY(void);
static uint8_t PHP(void);
static uint8_t SEC(void);
static uint8_t CLC(void);
static uint8_t CLI(void);
static uint8_t PLP(void);
static uint8_t PLA(void);
static uint8_t PHA(void);
static uint8_t SEI(void);
static uint8_t TYA(void);
static uint8_t CLV(void);
static uint8_t CLD(void);
static uint8_t SED(void);
static uint8_t TXA(void);
static uint8_t TXS(void);
static uint8_t TAX(void);
static uint8_t TAY(void);
static uint8_t TSX(void);
static uint8_t JMP(void);

// opcode lookup table

struct Instruction lookup[256] = {
    {"BRK", &BRK, &IMM, 7}, {"ORA", &ORA, &IZX, 6}, {"???", &XXX, &IMP, 2},
    {"???", &XXX, &IMP, 8}, {"???", &NOP, &IMP, 3}, {"ORA", &ORA, &ZP0, 3},
    {"ASL", &ASL, &ZP0, 5}, {"???", &XXX, &IMP, 5}, {"PHP", &PHP, &IMP, 3},
    {"ORA", &ORA, &IMM, 2}, {"ASL", &ASL, &IMP, 2}, {"???", &XXX, &IMP, 2},
    {"???", &NOP, &IMP, 4}, {"ORA", &ORA, &ABS, 4}, {"ASL", &ASL, &ABS, 6},
    {"???", &XXX, &IMP, 6}, {"BPL", &BPL, &REL, 2}, {"ORA", &ORA, &IZY, 5},
    {"???", &XXX, &IMP, 2}, {"???", &XXX, &IMP, 8}, {"???", &NOP, &IMP, 4},
    {"ORA", &ORA, &ZPX, 4}, {"ASL", &ASL, &ZPX, 6}, {"???", &XXX, &IMP, 6},
    {"CLC", &CLC, &IMP, 2}, {"ORA", &ORA, &ABY, 4}, {"???", &NOP, &IMP, 2},
    {"???", &XXX, &IMP, 7}, {"???", &NOP, &IMP, 4}, {"ORA", &ORA, &ABX, 4},
    {"ASL", &ASL, &ABX, 7}, {"???", &XXX, &IMP, 7}, {"JSR", &JSR, &ABS, 6},
    {"AND", &AND, &IZX, 6}, {"???", &XXX, &IMP, 2}, {"???", &XXX, &IMP, 8},
    {"BIT", &BIT, &ZP0, 3}, {"AND", &AND, &ZP0, 3}, {"ROL", &ROL, &ZP0, 5},
    {"???", &XXX, &IMP, 5}, {"PLP", &PLP, &IMP, 4}, {"AND", &AND, &IMM, 2},
    {"ROL", &ROL, &IMP, 2}, {"???", &XXX, &IMP, 2}, {"BIT", &BIT, &ABS, 4},
    {"AND", &AND, &ABS, 4}, {"ROL", &ROL, &ABS, 6}, {"???", &XXX, &IMP, 6},
    {"BMI", &BMI, &REL, 2}, {"AND", &AND, &IZY, 5}, {"???", &XXX, &IMP, 2},
    {"???", &XXX, &IMP, 8}, {"???", &NOP, &IMP, 4}, {"AND", &AND, &ZPX, 4},
    {"ROL", &ROL, &ZPX, 6}, {"???", &XXX, &IMP, 6}, {"SEC", &SEC, &IMP, 2},
    {"AND", &AND, &ABY, 4}, {"???", &NOP, &IMP, 2}, {"???", &XXX, &IMP, 7},
    {"???", &NOP, &IMP, 4}, {"AND", &AND, &ABX, 4}, {"ROL", &ROL, &ABX, 7},
    {"???", &XXX, &IMP, 7}, {"RTI", &RTI, &IMP, 6}, {"EOR", &EOR, &IZX, 6},
    {"???", &XXX, &IMP, 2}, {"???", &XXX, &IMP, 8}, {"???", &NOP, &IMP, 3},
    {"EOR", &EOR, &ZP0, 3}, {"LSR", &LSR, &ZP0, 5}, {"???", &XXX, &IMP, 5},
    {"PHA", &PHA, &IMP, 3}, {"EOR", &EOR, &IMM, 2}, {"LSR", &LSR, &IMP, 2},
    {"???", &XXX, &IMP, 2}, {"JMP", &JMP, &ABS, 3}, {"EOR", &EOR, &ABS, 4},
    {"LSR", &LSR, &ABS, 6}, {"???", &XXX, &IMP, 6}, {"BVC", &BVC, &REL, 2},
    {"EOR", &EOR, &IZY, 5}, {"???", &XXX, &IMP, 2}, {"???", &XXX, &IMP, 8},
    {"???", &NOP, &IMP, 4}, {"EOR", &EOR, &ZPX, 4}, {"LSR", &LSR, &ZPX, 6},
    {"???", &XXX, &IMP, 6}, {"CLI", &CLI, &IMP, 2}, {"EOR", &EOR, &ABY, 4},
    {"???", &NOP, &IMP, 2}, {"???", &XXX, &IMP, 7}, {"???", &NOP, &IMP, 4},
    {"EOR", &EOR, &ABX, 4}, {"LSR", &LSR, &ABX, 7}, {"???", &XXX, &IMP, 7},
    {"RTS", &RTS, &IMP, 6}, {"ADC", &ADC, &IZX, 6}, {"???", &XXX, &IMP, 2},
    {"???", &XXX, &IMP, 8}, {"???", &NOP, &IMP, 3}, {"ADC", &ADC, &ZP0, 3},
    {"ROR", &ROR, &ZP0, 5}, {"???", &XXX, &IMP, 5}, {"PLA", &PLA, &IMP, 4},
    {"ADC", &ADC, &IMM, 2}, {"ROR", &ROR, &IMP, 2}, {"???", &XXX, &IMP, 2},
    {"JMP", &JMP, &IND, 5}, {"ADC", &ADC, &ABS, 4}, {"ROR", &ROR, &ABS, 6},
    {"???", &XXX, &IMP, 6}, {"BVS", &BVS, &REL, 2}, {"ADC", &ADC, &IZY, 5},
    {"???", &XXX, &IMP, 2}, {"???", &XXX, &IMP, 8}, {"???", &NOP, &IMP, 4},
    {"ADC", &ADC, &ZPX, 4}, {"ROR", &ROR, &ZPX, 6}, {"???", &XXX, &IMP, 6},
    {"SEI", &SEI, &IMP, 2}, {"ADC", &ADC, &ABY, 4}, {"???", &NOP, &IMP, 2},
    {"???", &XXX, &IMP, 7}, {"???", &NOP, &IMP, 4}, {"ADC", &ADC, &ABX, 4},
    {"ROR", &ROR, &ABX, 7}, {"???", &XXX, &IMP, 7}, {"???", &NOP, &IMP, 2},
    {"STA", &STA, &IZX, 6}, {"???", &NOP, &IMP, 2}, {"???", &XXX, &IMP, 6},
    {"STY", &STY, &ZP0, 3}, {"STA", &STA, &ZP0, 3}, {"STX", &STX, &ZP0, 3},
    {"???", &XXX, &IMP, 3}, {"DEY", &DEY, &IMP, 2}, {"???", &NOP, &IMP, 2},
    {"TXA", &TXA, &IMP, 2}, {"???", &XXX, &IMP, 2}, {"STY", &STY, &ABS, 4},
    {"STA", &STA, &ABS, 4}, {"STX", &STX, &ABS, 4}, {"???", &XXX, &IMP, 4},
    {"BCC", &BCC, &REL, 2}, {"STA", &STA, &IZY, 6}, {"???", &XXX, &IMP, 2},
    {"???", &XXX, &IMP, 6}, {"STY", &STY, &ZPX, 4}, {"STA", &STA, &ZPX, 4},
    {"STX", &STX, &ZPY, 4}, {"???", &XXX, &IMP, 4}, {"TYA", &TYA, &IMP, 2},
    {"STA", &STA, &ABY, 5}, {"TXS", &TXS, &IMP, 2}, {"???", &XXX, &IMP, 5},
    {"???", &NOP, &IMP, 5}, {"STA", &STA, &ABX, 5}, {"???", &XXX, &IMP, 5},
    {"???", &XXX, &IMP, 5}, {"LDY", &LDY, &IMM, 2}, {"LDA", &LDA, &IZX, 6},
    {"LDX", &LDX, &IMM, 2}, {"???", &XXX, &IMP, 6}, {"LDY", &LDY, &ZP0, 3},
    {"LDA", &LDA, &ZP0, 3}, {"LDX", &LDX, &ZP0, 3}, {"???", &XXX, &IMP, 3},
    {"TAY", &TAY, &IMP, 2}, {"LDA", &LDA, &IMM, 2}, {"TAX", &TAX, &IMP, 2},
    {"???", &XXX, &IMP, 2}, {"LDY", &LDY, &ABS, 4}, {"LDA", &LDA, &ABS, 4},
    {"LDX", &LDX, &ABS, 4}, {"???", &XXX, &IMP, 4}, {"BCS", &BCS, &REL, 2},
    {"LDA", &LDA, &IZY, 5}, {"???", &XXX, &IMP, 2}, {"???", &XXX, &IMP, 5},
    {"LDY", &LDY, &ZPX, 4}, {"LDA", &LDA, &ZPX, 4}, {"LDX", &LDX, &ZPY, 4},
    {"???", &XXX, &IMP, 4}, {"CLV", &CLV, &IMP, 2}, {"LDA", &LDA, &ABY, 4},
    {"TSX", &TSX, &IMP, 2}, {"???", &XXX, &IMP, 4}, {"LDY", &LDY, &ABX, 4},
    {"LDA", &LDA, &ABX, 4}, {"LDX", &LDX, &ABY, 4}, {"???", &XXX, &IMP, 4},
    {"CPY", &CPY, &IMM, 2}, {"CMP", &CMP, &IZX, 6}, {"???", &NOP, &IMP, 2},
    {"???", &XXX, &IMP, 8}, {"CPY", &CPY, &ZP0, 3}, {"CMP", &CMP, &ZP0, 3},
    {"DEC", &DEC, &ZP0, 5}, {"???", &XXX, &IMP, 5}, {"INY", &INY, &IMP, 2},
    {"CMP", &CMP, &IMM, 2}, {"DEX", &DEX, &IMP, 2}, {"???", &XXX, &IMP, 2},
    {"CPY", &CPY, &ABS, 4}, {"CMP", &CMP, &ABS, 4}, {"DEC", &DEC, &ABS, 6},
    {"???", &XXX, &IMP, 6}, {"BNE", &BNE, &REL, 2}, {"CMP", &CMP, &IZY, 5},
    {"???", &XXX, &IMP, 2}, {"???", &XXX, &IMP, 8}, {"???", &NOP, &IMP, 4},
    {"CMP", &CMP, &ZPX, 4}, {"DEC", &DEC, &ZPX, 6}, {"???", &XXX, &IMP, 6},
    {"CLD", &CLD, &IMP, 2}, {"CMP", &CMP, &ABY, 4}, {"NOP", &NOP, &IMP, 2},
    {"???", &XXX, &IMP, 7}, {"???", &NOP, &IMP, 4}, {"CMP", &CMP, &ABX, 4},
    {"DEC", &DEC, &ABX, 7}, {"???", &XXX, &IMP, 7}, {"CPX", &CPX, &IMM, 2},
    {"SBC", &SBC, &IZX, 6}, {"???", &NOP, &IMP, 2}, {"???", &XXX, &IMP, 8},
    {"CPX", &CPX, &ZP0, 3}, {"SBC", &SBC, &ZP0, 3}, {"INC", &INC, &ZP0, 5},
    {"???", &XXX, &IMP, 5}, {"INX", &INX, &IMP, 2}, {"SBC", &SBC, &IMM, 2},
    {"NOP", &NOP, &IMP, 2}, {"???", &SBC, &IMP, 2}, {"CPX", &CPX, &ABS, 4},
    {"SBC", &SBC, &ABS, 4}, {"INC", &INC, &ABS, 6}, {"???", &XXX, &IMP, 6},
    {"BEQ", &BEQ, &REL, 2}, {"SBC", &SBC, &IZY, 5}, {"???", &XXX, &IMP, 2},
    {"???", &XXX, &IMP, 8}, {"???", &NOP, &IMP, 4}, {"SBC", &SBC, &ZPX, 4},
    {"INC", &INC, &ZPX, 6}, {"???", &XXX, &IMP, 6}, {"SED", &SED, &IMP, 2},
    {"SBC", &SBC, &ABY, 4}, {"NOP", &NOP, &IMP, 2}, {"???", &XXX, &IMP, 7},
    {"???", &NOP, &IMP, 4}, {"SBC", &SBC, &ABX, 4}, {"INC", &INC, &ABX, 7},
    {"???", &XXX, &IMP, 7},
};

// absolute addr in memory
uint16_t addr_abs = 0x0000;

// relative addr in memory
uint16_t addr_rel = 0x0000;

uint8_t op = 0x00;
uint32_t *cys = 0x000000;

uint8_t fetched = 0x00;

// Helper Functions

/*
    branch: execs a branch
*/
static void branch(void) {
  (*cys)++;
  addr_abs = cpu.pc + addr_rel;
  if ((addr_abs & 0xFF00) != (cpu.pc & 0xff00))
    (*cys)++;
  cpu.pc = addr_abs;
  debug_print("(branch) we are at 0x%X\n", cpu.pc);
}

/*
    fetch: cpu_fetch based wrapper
*/
static void fetch(void) {
  if (lookup[op].mode != &IMP)
    fetched = cpu_fetch(addr_abs);
}

/*
    set_flag: sets or unsets sr
*/
static void set_flag(uint8_t flag, bool exp) {
  if (exp)
    cpu_mod_sr(flag, 1);
  else
    cpu_mod_sr(flag, 0);
}

/*
    reset: resets cpu
*/
void reset(void) {
  addr_abs = 0x8000;
  cpu.pc = addr_abs;
  debug_print("(reset) pc: 0x%X\n", cpu.pc);
  cpu.ac = 0;
  cpu.x = 0;
  cpu.y = 0;
  cpu.sp = 0xFD;
  cpu.sr = 0x00;

  addr_rel = 0x0000;
  addr_abs = 0x0000;
  fetched = 0x00;
}

// modes
// return 1 if extra clock required

/*
    IMP: Implicit Mode.
*/
static uint8_t IMP(void) {
  fetched = cpu.ac;
  return 0;
}

/*
  IMM: Immediate Mode. Constants can be specified directly
  such as LDA 10 => Load 10 to accumulator
*/
static uint8_t IMM(void) {
  addr_abs = cpu.pc++;
  return 0;
}

/*
  ZP0: Zero Page Mode. Addresses within the 0th page can be specified
  only the first 256 bytes of memory (0xFF)
*/
static uint8_t ZP0(void) {
  addr_abs = (cpu_fetch(cpu.pc) & 0x00FF);
  return 0;
}

/*
  ZPX: ZP0 + cpu.x
*/
static uint8_t ZPX(void) {
  addr_abs = ((cpu_fetch(cpu.pc) + cpu.x) & 0x00FF);
  return 0;
}

/*
  ZPY: ZP0 + cpu.y
*/
static uint8_t ZPY(void) {
  addr_abs = ((cpu_fetch(cpu.pc) + cpu.y) & 0x00FF);
  return 0;
}

/*
  ABS: Absolute Mode. Full 16bit addr specified.
*/
static uint8_t ABS(void) {
  uint16_t low = cpu_fetch(cpu.pc);
  uint16_t high = cpu_fetch(cpu.pc);
  addr_abs = (high << 8) | low;
  return 0;
}

/*
  ABX: ABS + cpu.x
  returns 1 if page change (extra cycle required)
*/
static uint8_t ABX(void) {
  uint16_t low = cpu_fetch(cpu.pc);
  uint16_t high = cpu_fetch(cpu.pc);
  addr_abs = (high << 8) | low;
  addr_abs += cpu.x;
  // if overflow => page changed => return 1
  return ((addr_abs & 0xFF00) != (high << 8)) ? 1 : 0;
}

/*
  ABY: ABS + cpu.y
  returns 1 if page change (extra cycle required)
*/
static uint8_t ABY(void) {
  uint16_t low = cpu_fetch(cpu.pc);
  uint16_t high = cpu_fetch(cpu.pc);
  addr_abs = (high << 8) | low;
  addr_abs += cpu.y;
  // if overflow => page changed => return 1
  return ((addr_abs & 0xFF00) != (high << 8)) ? 1 : 0;
}

/*
  IND: Indirect Addressing Mode. Basically 6502 pointers.
  Only used by JMP
*/
static uint8_t IND(void) {
  uint16_t low = cpu_fetch(cpu.pc);
  uint16_t high = cpu_fetch(cpu.pc);
  uint16_t ptr = (high << 8) | low;

  if (low == 0x00FF) {
    // Simulating Hardware Bug. https://www.nesdev.com/6502bugs.txt
    // "An indirect JMP (xxFF) will fail because the MSB will be fetched from
    // address xx00 instead of page xx+1."
    addr_abs = (cpu_fetch(ptr & 0xFF00) << 8) | cpu_fetch(ptr + 0);
  } else {
    addr_abs = (cpu_fetch(ptr + 1) << 8) | cpu_fetch(ptr + 0);
  }

  return 0;
}

/*
  IZX: Indirect Addressing of 0th page with offset X.
*/
static uint8_t IZX(void) {
  uint16_t addr_zero_page = cpu_fetch(cpu.pc);
  uint16_t low =
      cpu_fetch((uint16_t)(addr_zero_page + (uint16_t)cpu.x) & 0x00FF);
  uint16_t high =
      cpu_fetch((uint16_t)(addr_zero_page + (uint16_t)cpu.x + 1) & 0x00FF);
  addr_abs = (high << 8) | low;

  return 0;
}

/*
  IZX: Indirect Addressing of 0th page with offset Y.
  THIS WORKS DIFFERENT FROM IZX
*/
static uint8_t IZY(void) {
  uint16_t addr_zero_page = cpu_fetch(cpu.pc);
  uint16_t low = cpu_fetch(addr_zero_page & 0x00FF);
  uint16_t high = cpu_fetch((addr_zero_page + 1) & 0x00FF);
  addr_abs = (high << 8) | low;
  addr_abs += cpu.y;
  return ((addr_abs & 0xFF00) != (high << 8)) ? 1 : 0;
}

/*
  REL: Relative Addressing Mode.
  Used by branch instructions. The byte after the opcode is the branch offset.
  If the branch is taken, the new address will the the current PC plus the
  offset. Offset is signed 8bit (-127 to 128)
*/
static uint8_t REL(void) {
  addr_rel = cpu_fetch(cpu.pc);
  if (addr_rel & 0x80) {
    addr_rel |= 0xFF00;
  }
  return 0;
}

// operations

/*
  XXX: Unknown
*/
static uint8_t XXX(void) { return 0; }

/*
  LDA: Load Accumulator
*/
static uint8_t LDA(void) {
  fetch();
  cpu.ac = fetched;
  set_flag(Z, cpu.ac == 0);
  set_flag(N, cpu.ac & (1 << 7));
  return 1;
}

/*
  LDX: Load X Register
*/
static uint8_t LDX(void) {
  fetch();
  cpu.x = fetched;
  set_flag(Z, cpu.x == 0);
  set_flag(N, cpu.x & (1 << 7));
  return 1;
}

/*
  LDY: Load X Register
*/
static uint8_t LDY(void) {
  fetch();
  cpu.y = fetched;
  set_flag(Z, cpu.y == 0);
  set_flag(N, cpu.y & (1 << 7));
  return 1;
}

/*
  BRK: Break
  Can't quite figure this out
*/
static uint8_t BRK(void) {
  // Setting Interrupt Flag
  cpu.pc++;
  set_flag(I, true);

  // Pushing PC+2, providing an extra byte of spacing for a break mark
  cpu_write(0x0100 + cpu.sp, (cpu.pc >> 8) & 0x00FF);
  cpu.sp--;
  cpu_write(0x0100 + cpu.sp, cpu.pc & 0x00FF);
  cpu.sp--;

  // Pushing Status Register
  set_flag(B, true);
  cpu_write(0x0100 + cpu.sp, cpu.sr);
  cpu.sp--;
  set_flag(B, false);

  // Setting PC
  cpu.pc = (uint16_t)cpu_fetch(0xFFFE) | ((uint16_t)cpu_fetch(0xFFFF) << 8);
  return 0;
}

/*
  JSR: Jump to New Location Saving Return Address
*/
static uint8_t JSR(void) {
  cpu.pc--; // return address

  // saving return addr to stack
  cpu_write(0x0100 + cpu.sp, (cpu.pc >> 8) & 0x00FF);
  cpu.sp--;
  cpu_write(0x0100 + cpu.sp, cpu.pc & 0x00FF);
  cpu.sp--;

  cpu.pc = addr_abs; // execute jump

  return 0;
}

/*
  RTI: Return from Interrupt
*/
static uint8_t RTI(void) {
  // pulling sr
  cpu.sp++;

  cpu.sr = cpu_fetch(0x0100 + cpu.sp);
  cpu.sr &= ~B; // ignoring B

  // pulling pc
  cpu.sp++;
  cpu.pc = (uint16_t)cpu_fetch(0x0100 + cpu.sp);
  cpu.sp++;
  cpu.pc |= (uint16_t)cpu_fetch(0x0100 + cpu.sp) << 8;

  return 0;
}

/*
  RTS: Return from Subroutine
*/
static uint8_t RTS(void) {
  // pulling pc
  cpu.sp++;
  cpu.pc = (uint16_t)cpu_fetch(0x0100 + cpu.sp);
  cpu.sp++;
  cpu.pc |= (uint16_t)cpu_fetch(0x0100 + cpu.sp) << 8;

  cpu.pc++;

  return 0;
}

/*
  NOP: No Operation
*/
static uint8_t NOP(void) {
  cpu.pc++;
  return 0;
}

/*
  BCC: Branch on Carry Clear
*/
static uint8_t BCC(void) {
  if (cpu_extract_sr(C) == 0) {
    branch();
  }
  return 0;
}

/*
  BCS: Branch on Carry Set
*/
static uint8_t BCS(void) {
  if (cpu_extract_sr(C) == 1) {
    branch();
  }
  return 0;
}

/*
  BEQ: Branch on Result Zero
*/
static uint8_t BEQ(void) {
  if (cpu_extract_sr(Z) == 0) {
    branch();
  }
  return 0;
}

/*
  BMI: Branch on Result Minus
*/
static uint8_t BMI(void) {
  if (cpu_extract_sr(N) == 1) {
    branch();
  }
  return 0;
}

/*
  BNE: Branch on Result Non-zero
*/
static uint8_t BNE(void) {
  if (cpu_extract_sr(Z) == 0) {
    branch();
  }
  return 0;
}

/*
  BVC: Branch on Overflow Clear
*/
static uint8_t BVC(void) {
  if (cpu_extract_sr(V) == 0) {
    branch();
  }
  return 0;
}

/*
  BVS: Branch on Overflow Set
*/
static uint8_t BVS(void) {
  if (cpu_extract_sr(V) == 1) {
    branch();
  }
  return 0;
}

/*
  CPX: Compare Memory and Index X
*/
static uint8_t CPX(void) {
  fetch();
  uint16_t tmp = (uint16_t)cpu.x - (uint16_t)fetched;

  set_flag(C, cpu.x >= fetched);
  set_flag(Z, (tmp & 0x00FF) == 0x0000);
  set_flag(N, tmp & (1 << 7));

  return 0;
}

/*
  CPY: Compare Memory and Index Y
*/
static uint8_t CPY(void) {
  fetch();
  uint16_t tmp = (uint16_t)cpu.y - (uint16_t)fetched;

  set_flag(C, cpu.y >= fetched);
  set_flag(Z, (tmp & 0x00FF) == 0x0000);
  set_flag(N, tmp & (1 << 7));

  return 0;
}

/*
  ORA: OR Memory with Accumulator
*/
static uint8_t ORA(void) {
  fetch();
  cpu.ac = cpu.ac | fetched;

  set_flag(C, cpu.ac == 0);
  set_flag(N, cpu.ac & (1 << 7));

  return 1;
}

/*
  AND: AND Memory with Accumulator
*/
static uint8_t AND(void) {
  fetch();
  cpu.ac = cpu.ac & fetched;

  set_flag(C, cpu.ac == 0);
  set_flag(N, cpu.ac & (1 << 7));

  return 1;
}

/*
  EOR: Exclusive-OR Memory with Accumulator
*/
static uint8_t EOR(void) {
  fetch();
  cpu.ac = cpu.ac ^ fetched;

  set_flag(C, cpu.ac == 0);
  set_flag(N, cpu.ac & (1 << 7));

  return 1;
}

/*
  BIT: Test Bits in Memory with Accumulator
  bits 7 and 6 of operand are transfered to bit 7 and 6 of SR (N,V);
  the zero-flag is set to the result of operand AND accumulator.
*/
static uint8_t BIT(void) {
  fetch();
  uint16_t tmp = cpu.ac & fetched;

  set_flag(Z, (tmp & 0x00F) == 0x00);
  set_flag(N, (fetched & (1 << 7)));
  set_flag(V, (fetched & (1 << 6)));

  return 0;
}

/*
  ADC: Add Memory to Accumulator with Carry
*/
static uint8_t ADC(void) {
  fetch();

  uint16_t tmp =
      (uint16_t)cpu.ac + (uint16_t)fetched + (uint16_t)cpu_extract_sr(C);

  set_flag(C, tmp > 0xFF);
  set_flag(Z, (tmp & 0x00FF) == 0);
  set_flag(V, ((~((uint16_t)cpu.ac ^ (uint16_t)fetched) &
                ((uint16_t)cpu.ac ^ (uint16_t)tmp)) &
               0x0080));

  set_flag(N, tmp & 0x0080);

  cpu.ac = tmp & 0x00FF;
  return 1;
}

/*
  STA: Store Accumulator in Memory
*/
static uint8_t STA(void) {
  cpu_write(addr_abs, cpu.ac);
  return 0;
}

/*
  STX: Store Index X in Memory
*/
static uint8_t STX(void) {
  cpu_write(addr_abs, cpu.x);
  return 0;
}

/*
  STY: Store Index Y in Memory
*/
static uint8_t STY(void) {
  cpu_write(addr_abs, cpu.y);
  return 0;
}

/*
  CMP: Compare Memory with Accumulator
*/
static uint8_t CMP(void) {
  fetch();

  uint16_t tmp = (uint16_t)cpu.ac - (uint16_t)fetched;

  set_flag(C, cpu.ac >= fetched);
  set_flag(Z, (tmp & 0x00FF) == 0x0000);
  set_flag(N, tmp & (1 << 7));

  return 1;
}

/*
  SBC: Subtract Memory from Accumulator with Borrow
*/
static uint8_t SBC(void) {
  fetch();
  uint16_t val = ((uint16_t)fetched) ^ 0x00FF; // complementing last 8 bits

  uint16_t tmp = (uint16_t)cpu.ac + val + (uint16_t)cpu_extract_sr(C);

  // set flags
  set_flag(C, tmp & 0xFF00);
  set_flag(Z, (tmp & 0x00FF) == 0);
  set_flag(V, ((tmp ^ (uint16_t)cpu.ac) & (tmp ^ val) & 0x0080));
  set_flag(N, tmp & 0x0080);

  // only get the last 8 bits
  cpu.ac = tmp & 0x00FF;
  return 1;
}

/*
  ASL: Arithmetic Shift Left
*/
static uint8_t ASL(void) {
  fetch();
  uint16_t tmp = (uint16_t)fetched << 1;

  // set appropriate flags
  set_flag(C, (tmp & 0xFF00) > 0);
  set_flag(Z, (tmp & 0x00FF) == 0x00);
  set_flag(N, tmp & (1 << 7));

  // write acc to addressing mode
  if (lookup[op].mode == &IMP) {
    cpu.ac = tmp & 0x00FF;
  } else {
    cpu_write(addr_abs, tmp & 0x00FF);
  }
  return 0;
}

/*
  ROL: Rotate Left
*/
static uint8_t ROL(void) {
  fetch();
  uint16_t tmp = (uint16_t)(fetched << 1) | cpu_extract_sr(C);

  // set appropriate flags
  set_flag(C, tmp & 0xFF00);
  set_flag(Z, (tmp & 0x00FF) == 0x00);
  set_flag(N, tmp & (1 << 7));

  // write acc to addressing mode
  if (lookup[op].mode == &IMP) {
    cpu.ac = tmp & 0x00FF;
  } else {
    cpu_write(addr_abs, tmp & 0x00FF);
  }
  return 0;
}

/*
  LSR: Logical Shift Right
*/
static uint8_t LSR(void) {
  fetch();
  uint16_t tmp = (uint16_t)fetched >> 1;

  // set appropriate flags
  set_flag(C, fetched & 0x0001);
  set_flag(Z, (tmp & 0x00FF) == 0x00);
  set_flag(N, tmp & (1 << 7));

  // set appropriate flags
  if (lookup[op].mode == &IMP) {
    cpu.ac = tmp & 0x00FF;
  } else {
    cpu_write(addr_abs, tmp & 0x00FF);
  }
  return 0;
}

/*
  DEC: Decrement Memory
*/
static uint8_t DEC(void) {
  fetch();
  uint16_t tmp = fetched - 1;

  cpu_write(addr_abs, tmp & 0x00FF);
  set_flag(Z, ((tmp & 0x00FF) == 0x0000));
  set_flag(N, (tmp & (1 << 7)));
  return 0;
}

/*
  DEX: Decrement X
*/
static uint8_t DEX(void) {
  cpu.x--;

  set_flag(Z, cpu.x == 0x00);
  set_flag(N, cpu.x & (1 << 7));
  return 0;
}

/*
  DEY: Decrement X
*/
static uint8_t DEY(void) {
  cpu.y--;

  set_flag(Z, cpu.y == 0x00);
  set_flag(N, cpu.y & (1 << 7));
  return 0;
}

/*
  INC: Increment Memory
*/
static uint8_t INC(void) {
  fetch();
  uint16_t tmp = (uint16_t)fetched + 1;

  cpu_write(addr_abs, tmp & 0x00FF);
  set_flag(Z, ((tmp & 0x00FF) == 0x0000));
  set_flag(N, tmp & (1 << 7));
  return 0;
}

/*
  INX: Increment X
*/
static uint8_t INX(void) {
  cpu.x++;

  set_flag(Z, cpu.x == 0x00);
  set_flag(N, cpu.x & (1 << 7));
  return 0;
}

/*
  INY: Increment Y
*/
static uint8_t INY(void) {
  cpu.x++;

  set_flag(Z, cpu.x == 0x00);
  set_flag(N, cpu.x & (1 << 7));
  return 0;
}

/*
  PHP: Push Processor Status on Stack
*/
static uint8_t PHP(void) {
  cpu_write(0x0100 + cpu.sp, cpu.sr);

  cpu.sp--;
  return 0;
}

/*
  SEC: Set Carry Flag
*/
static uint8_t SEC(void) {
  set_flag(C, true);
  return 0;
}

/*
  CLC: Clear Carry Flag
*/
static uint8_t CLC(void) {
  set_flag(C, false);
  return 0;
}

/*
  PLP: Pull Processor Status from Stack
*/
static uint8_t PLP(void) {
  cpu.sp++;
  cpu.sr = cpu_fetch(0x0100 + cpu.sp);

  return 0;
}

/*
  PLA: Pull Accumulator from Stack
*/
static uint8_t PLA(void) {
  cpu.sp++;
  cpu.ac = cpu_fetch(0x0100 + cpu.sp);

  set_flag(Z, cpu.ac == 0);
  set_flag(N, cpu.ac & (1 << 7));
  return 0;
}

/*
  PHA: Push Accumulator on Stack
*/
static uint8_t PHA(void) {
  cpu_write(0x0100 + cpu.sp, cpu.ac);

  cpu.sp--;
  return 0;
}

/*
  CLI: Clear Interrupt Disable Bit
*/
static uint8_t CLI(void) {
  set_flag(I, 0);
  return 0;
}

/*
  SEI: Set Interrupt Disable Status
*/
static uint8_t SEI(void) {
  set_flag(I, 1);
  return 0;
}

/*
  TXA: Transfer Index X to Accumulator
*/
static uint8_t TXA(void) {
  cpu.ac = cpu.x;

  set_flag(Z, cpu.ac == 0);
  set_flag(N, cpu.ac & (1 << 7));
  return 0;
}

/*
  TYA: Transfer Index Y to Accumulator
*/
static uint8_t TYA(void) {
  cpu.ac = cpu.y;

  set_flag(Z, cpu.ac == 0);
  set_flag(N, cpu.ac & (1 << 7));
  return 0;
}

/*
  CLV: Clear Overflow Flag
*/
static uint8_t CLV(void) {
  set_flag(V, 0);
  return 0;
}

/*
  CLD: Clear Decimal Mode
*/
static uint8_t CLD(void) {
  set_flag(D, 0);
  return 0;
}

/*
  SLD: Set Decimal Mode
*/
static uint8_t SLD(void) {
  set_flag(D, 1);
  return 0;
}

/*
  TAX: Transfer Accumulator to Index X
*/
static uint8_t TAX(void) {
  cpu.x = cpu.ac;

  set_flag(Z, cpu.x == 0);
  set_flag(N, (cpu.x & (1 << 7)));
  return 0;
}

/*
  TAY: Transfer Accumulator to Index Y
*/
static uint8_t TAY(void) {
  cpu.y = cpu.ac;

  set_flag(Z, cpu.y == 0);
  set_flag(N, (cpu.y & (1 << 7)));
  return 0;
}

/*
  TSX: Transfer Stack Pointer to Index X
*/
static uint8_t TSX(void) {
  cpu.x = cpu.sp;

  set_flag(Z, cpu.x == 0);
  set_flag(N, (cpu.x & (1 << 7)));
  return 0;
}

/*
  JMP: Jump to New Location
*/
static uint8_t JMP(void) {
  cpu.pc = addr_abs;
  return 0;
}

// Instruction Parsing and Execution
void instruction_exec(uint8_t opcode, uint32_t *cycles) {
  op = opcode;
  cys = cycles;
  *cycles = lookup[opcode].cycles;

  uint8_t extra_cycle_0 = (*(lookup[opcode].mode))();
  uint8_t extra_cycle_1 = (*(lookup[opcode].op))();

  *cycles += (extra_cycle_0 & extra_cycle_1);
  debug_print("(instruction_exec) cycles: %d, %p\n", *(cycles), (void *)cycles);
}