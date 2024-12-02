
#include <dlfcn.h>

#include <vcs.h>
uint32_t rst_debug  ,
pc_debug   ,
inst_debug ,
valid_debug;

void debug_info(int pc,int inst,int valid,int rst,int wen,int waddr,int wdata){
    rst_debug   = rst;
    pc_debug    = pc;
    inst_debug  = inst;
    valid_debug = valid;
    if((valid==1&&(wen==1))){
        // printf("cpu en %08x addr %d data %08x\n",wen,waddr,wdata);
        cpu.gpr[waddr]=wdata;

    }
}
///////////////////////////////////////////////////////
CPU_state cpu={};
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

////////////////////////DEVICE_BASE///////////////////

//////////////////////////////////////////////////////

void isa_reg_display();
void (*ref_difftest_memcpy)(paddr_t addr, void *buf, size_t n, bool direction) = NULL;
void (*ref_difftest_regcpy)(void *dut, bool direction) = NULL;
void (*ref_difftest_exec)(uint64_t n) = NULL;
void (*ref_difftest_raise_intr)(uint64_t NO) = NULL;
bool initial=false;
void init_difftest(char *ref_so_file, long img_size, int port) {
  assert(ref_so_file != NULL);

  void *handle;
  handle = dlopen(ref_so_file, RTLD_LAZY);
  assert(handle);

  // ref_difftest_memcpy = dlsym(handle, "difftest_memcpy");
  ref_difftest_memcpy = reinterpret_cast<void (*)(paddr_t, void*, size_t,bool)>(dlsym(handle, "difftest_memcpy"));
  assert(ref_difftest_memcpy);

  // ref_difftest_regcpy = dlsym(handle, "difftest_regcpy");
  ref_difftest_regcpy= reinterpret_cast<void (*)(void*,bool)>(dlsym(handle, "difftest_regcpy"));
  assert(ref_difftest_regcpy);

  // ref_difftest_exec = dlsym(handle, "difftest_exec");
  ref_difftest_exec = reinterpret_cast<void (*)(uint64_t)>(dlsym(handle, "difftest_exec"));
  assert(ref_difftest_exec);

  // ref_difftest_raise_intr = dlsym(handle, "difftest_raise_intr");
  ref_difftest_raise_intr = reinterpret_cast<void (*)(uint64_t)>(dlsym(handle, "difftest_raise_intr"));
  assert(ref_difftest_raise_intr);

  // void (*ref_difftest_init)(int) = dlsym(handle, "difftest_init");
  void (*ref_difftest_init)(int) = reinterpret_cast<void (*)(int)>(dlsym(handle, "difftest_init"));
  assert(ref_difftest_init);

  Log("Differential testing: %s", ANSI_FMT("ON", ANSI_FG_GREEN));
  Log("The result of every instruction will be compared with %s. "
      "This will help you a lot for debugging, but also significantly reduce the performance. "
      "If it is not necessary, you can turn it off in menuconfig.", ref_so_file);
  ref_difftest_init(port);
  uint8_t *ptr8 = reinterpret_cast<uint8_t*>(mem);
  ref_difftest_memcpy(RESET_VECTOR, ptr8,CONFIG_MSIZE, DIFFTEST_TO_REF);
  ref_difftest_regcpy(&cpu, DIFFTEST_TO_REF);
}
bool isa_difftest_checkregs(CPU_state *ref_r, vaddr_t pc) {
  int i;
  bool reg_check=true;
  for(i=0;i<MUXDEF(CONFIG_RVE, 16, 32);i++){
      const char *s=regs[i];
      reg_check=difftest_check_reg(s,ref_r->pc,ref_r->gpr[i],cpu.gpr[i]);
      if(reg_check==false){
        return reg_check;
      }
  }
  return reg_check;
}

static void checkregs(CPU_state *ref, vaddr_t pc) {
  if (!isa_difftest_checkregs(ref, pc)) {
    isa_reg_display();
    assert(0);
    exit(0);
  }
}

bool end=false;
void difftest_pipeline(int pc,int finish) {
  
  if(finish){
    end= true;
  }else{
    end= false;
  }
}
bool time1=false;
void difftest_step(vaddr_t pc, vaddr_t npc,bool diff_mode,bool *end) {

  CPU_state ref_r;
  if(diff_mode==false){
    return ;
  }
  else if((end)){
    ref_difftest_exec(inst_num);
    ref_difftest_regcpy(&ref_r, DIFFTEST_TO_DUT);
    checkregs(&ref_r, pc);
  }
}