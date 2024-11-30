#ifndef ISA_H
#define ISA_H
#include <common.h>
typedef struct {
  word_t gpr[MUXDEF(CONFIG_RVE, 16, 32)];
  vaddr_t pc;
} CPU_state;

#endif