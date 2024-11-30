#include <init.h>
#include <common.h>
#include <GlobalVarible.h>
const char *regs[] = {
  "$0", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
  "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
  "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
  "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
};

void isa_reg_display() {
  int i=0;
  printf("\t%s\t",ANSI_FMT(str(reg), ANSI_FG_YELLOW ANSI_BG_RED));
  printf("\t");
  printf("\t%s\n",ANSI_FMT(str(data), ANSI_FG_YELLOW ANSI_BG_RED));
  for(i=0;i<MUXDEF(CONFIG_RVE, 16, 32);i++)
  {
    printf("\t%s\t",regs[i]);
    printf("\t");
    printf("\t0x%08x\n",cpu.gpr[i]);
  }
}