
#include <dlfcn.h>
#include <init.h>
#include <difftest.h>
#include <GlobalVarible.h>
#include <performance.h>
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
      // printf("reg(%d)%08x\n",i,cpu.gpr[i]);
      if(reg_check==false){
        return reg_check;
      }
  }
  return reg_check;
}

static void checkregs(CPU_state *ref, vaddr_t pc) {
  if (!isa_difftest_checkregs(ref, pc)) {
    isa_reg_display();
    #ifdef  CONFIG_WAVE_TRECE
    tfp->close();
    #endif
    assert(0);
    exit(0);
  }
}
bool timer=false;
bool diff_stage1=false;
vaddr_t inst_num=0;
vaddr_t mempc;
void difftest_pipeline(int pc,int finish) {
  mempc=pc;
  Stat::ideal_instructions +=1;
  if(finish){
    timer=true;
    inst_num=1;
    Stat::executed_instructions+=1;
  }else{
    inst_num=0;
  }
}
bool time1=false;
// void super_difftest_step(vaddr_t pc1, vaddr_t pc2,int valid,int data1,int data2,int num){

// } 
void difftest_nstep(int step){
  int wen  = commit.commit_wen;
  int waddr= commit.commit_addr; 
  int wdata= commit.commit_data;
  int pc   = commit.commit_pc;

  for(int i=0;i<step;i++){
    if(wen&&0x1){
      cpu.gpr[waddr&0x1f] = wdata&0xffffffff;
    }
    difftest_step(pc&0xffffffff,0,true);
    wen = wen>>1;
    wdata = wdata>>32;
    waddr = waddr>>5;
    pc    = pc>>32;
  }
}
void difftest_step(vaddr_t pc, vaddr_t npc,bool diff_mode) {
  // if(inst_num==1){
  //   printf("diff\n");
  // }
  CPU_state ref_r;
  if(diff_mode==false){
    return ;
  }
  else if((inst_num)){
    if((diff_amend==true)){
      ref_difftest_exec(inst_num);
      ref_difftest_regcpy(&cpu, DIFFTEST_TO_REF);
      diff_amend=false;
      diff_stage1=true;
      //printf("diffref%08x dut %08x\n",ref_r.pc,cpu.pc);
    }
    else{
      if(diff_stage1==true){
        ref_difftest_exec(inst_num);
        cpu.pc+=4;
        ref_difftest_regcpy(&cpu, DIFFTEST_TO_REF);
        //ref_difftest_regcpy(&ref_r, DIFFTEST_TO_DUT);
        //checkregs(&ref_r, pc);
        
        diff_stage1=false;
      }
      else{
        
        ref_difftest_exec(inst_num);
        ref_difftest_regcpy(&ref_r, DIFFTEST_TO_DUT);
        
        checkregs(&ref_r, pc);
      }
    }

  }
}