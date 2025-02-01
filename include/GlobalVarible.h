#ifndef __GLOBALVARIBLE_H__
#define __GLOBALVARIBLE_H__
#include <isa.h>
#define PMEM_LEFT  ((paddr_t)CONFIG_MBASE)
#define PMEM_RIGHT ((paddr_t)CONFIG_MBASE + CONFIG_MSIZE - 1)
#define RESET_VECTOR (PMEM_LEFT + CONFIG_PC_RESET_OFFSET)
#define Vaddr2Paddr(x)   (x-CONFIG_MBASE)
#define MemAccess(x)  (Vaddr2Paddr(x)) 
extern uint8_t mem[CONFIG_MSIZE];
extern uint8_t sram[0x10000];
extern uint32_t last_inst;
extern char logbuf[128];
extern CPU_state cpu;
extern Commit commit;
extern uint64_t InitTime;
extern const char *regs[];
extern bool  timer_addr;
extern bool  diff_amend;
#ifdef CONFIG_FTRACE
extern char Func[CONFIG_FUNCNUM][CONFIG_FUNCLINESIZE];
extern char Func_pc[CONFIG_FUNCNUM][9];
extern char Func_name[CONFIG_FUNCNUM][CONFIG_FUNCSIZE];
#endif
extern u_char rst_debug; 
extern word_t pc_debug;  
extern word_t inst_debug;
extern u_char valid_debug;
#endif