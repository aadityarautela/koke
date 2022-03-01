#include "mem.h"
#include "../utils/misc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

struct mem memory;

static void load_program(char *path) {
  FILE *fp = fopen(path, "rb");
  if (fp == NULL) {
    fprintf(stderr, "[ERROR] Can't open file.\n");
    exit(1);
  }

  struct stat st;
  stat(path, &st);
  size_t fsize = st.st_size;

  size_t read_bytes =
      fread(memory.data + (0x8000 - 0x0200), 1, sizeof(memory.data), fp);

  if (read_bytes != fsize) {
    fprintf(stderr, "[ERROR] Can't Read All Bytes.\n");
    exit(1);
  }
  fclose(fp);
}

void mem_init(char *filename) {
  memset(memory.zero_page, 0, sizeof(memory.zero_page));
  memset(memory.stack, 0, sizeof(memory.stack));
  memset(memory.data, 0, sizeof(memory.data));

  memory.last_six[0] = 0xA;
  memory.last_six[1] = 0xB;
  memory.last_six[2] = 0xC;
  memory.last_six[3] = 0xD;
  memory.last_six[4] = 0xE;
  memory.last_six[5] = 0xF;

  if (filename == NULL) {
    fprintf(stderr, "[ERROR] No filename provided.\n");
    exit(1);
  } else {
    load_program(filename);
  }
}

struct mem *mem_get_ptr() {
  struct mem *mp = &memory;
  return mp;
}

int mem_dump() {
  FILE *fp = fopen("dump.bin", "wb+");
  if (fp == NULL)
    return 1;
  size_t size_check = fwrite(memory.zero_page, 1, sizeof(memory.zero_page), fp);
  if (size_check != sizeof(memory.zero_page)) {
    printf("[ERROR] Error while dumping zero page.\n");
    fclose(fp);
    return 1;
  }
  size_check = fwrite(memory.stack, 1, sizeof(memory.stack), fp);
  if (size_check != sizeof(memory.stack)) {
    printf("[ERROR] Error while dumping stack.\n");
    fclose(fp);
    return 1;
  }
  size_check = fwrite(memory.data, 1, sizeof(memory.data), fp);
  if (size_check != sizeof(memory.data)) {
    printf("[ERROR] Error while dumping data.\n");
    fclose(fp);
    return 1;
  }
  size_check = fwrite(memory.last_six, 1, sizeof(memory.last_six), fp);
  if (size_check != sizeof(memory.last_six)) {
    printf("[ERROR] Error while dumping reserved last six bytes.\n");
    fclose(fp);
    return 1;
  }
  fclose(fp);
  return 0;
}