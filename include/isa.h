#ifndef ISA_H
#define ISA_H
#include <common.h>
typedef struct {
  word_t gpr[MUXDEF(CONFIG_RVE, 16, 32)];
  vaddr_t pc;
} CPU_state;
typedef struct {
  char      commit_wen  ;
  int       commit_addr ; 
  char      commit_num  ;
  long long commit_data ; 
  long long commit_pc   ;
}Commit;

#endif