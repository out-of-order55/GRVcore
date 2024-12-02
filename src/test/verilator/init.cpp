
#include <init.h>

#include <getopt.h>
#include <difftest.h>
#include <performance.h>
#include <time.h>
#include <GlobalVarible.h>
#define TOP top
// void Performance::performance();
#ifdef NVBOARD
#include <nvboard.h>
void nvboard_init(int vga_clk_cycle = 1);
void nvboard_quit();
void nvboard_update();
void nvboard_bind_all_pins(Vtop* top);
#endif
//variable definition
const std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};
const std::unique_ptr<Vtop> top{new Vtop{contextp.get(), "TOP"}};
#ifdef  CONFIG_WAVE_TRECE
VerilatedVcdC* tfp = new VerilatedVcdC;
#endif

extern void init_disasm(const char *triple);
extern void disassemble(char *str, int size, uint64_t pc, uint8_t *code, int nbyte);
void init_difftest(char *ref_so_file, long img_size, int port);
char logbuf[128];
uint8_t mem[CONFIG_MSIZE]={};
static char *img_file = NULL;
static char *log_file = NULL;
static char *elf_file=NULL;
static char *diff_so_file=NULL;
static int difftest_port = 1234;
FILE *log_fp = NULL;
CPU_state cpu={};
uint64_t  InitTime=0;
static uint64_t  start_time=0;
static uint64_t  end_time=0;
static uint64_t  g_timer=0;
static uint64_t g_nr_guest_inst=0;

////////////////////////////////////////////////////////

#include <sys/time.h>
int diff_flag=0;
bool is_batch_mode=false;
#ifdef CONFIG_FTRACE
char Func[CONFIG_FUNCNUM][CONFIG_FUNCLINESIZE]={};
char Func_pc[CONFIG_FUNCNUM][9]={};
char Func_name[CONFIG_FUNCNUM][CONFIG_FUNCSIZE]={};
static int Depth=0;
static int RetDepth=0;
typedef struct{
  char     FuncName[CONFIG_FUNCSIZE]; 
  uint32_t FuncDepth;
}Level;
Level Lv[CONFIG_FUNCNUM+500];
#endif
static void statistic() {
  IFNDEF(CONFIG_TARGET_AM, setlocale(LC_NUMERIC, ""));
#define NUMBERIC_FMT MUXDEF(CONFIG_TARGET_AM, "%", "%'") PRIu64
  Log("host time spent = " NUMBERIC_FMT " us", g_timer);
  Log("total guest instructions = " NUMBERIC_FMT, g_nr_guest_inst);
  if (g_timer > 0) Log("simulation frequency = " NUMBERIC_FMT " inst/s", g_nr_guest_inst * 1000000 / g_timer);
  else Log("Finish running in less than 1 us and can not calculate the simulation frequency");
}
static void welcome() {
  Log("ITrace: %s", MUXDEF(CONFIG_ITRACE, ANSI_FMT("ON", ANSI_FG_GREEN), ANSI_FMT("OFF", ANSI_FG_RED)));
  Log("MTrace: %s", MUXDEF(CONFIG_MTRACE, ANSI_FMT("ON", ANSI_FG_GREEN), ANSI_FMT("OFF", ANSI_FG_RED)));
  Log("FTrace: %s", MUXDEF(CONFIG_FTRACE, ANSI_FMT("ON", ANSI_FG_GREEN), ANSI_FMT("OFF", ANSI_FG_RED)));
  Log("ETrace: %s", MUXDEF(CONFIG_ETRACE, ANSI_FMT("ON", ANSI_FG_GREEN), ANSI_FMT("OFF", ANSI_FG_RED)));
  printf("Welcome to %s-NPC!\n", ANSI_FMT(str(riscv32), ANSI_BG_BLUE ANSI_BG_MAGENTA));
}
void TimeInit() {
  struct timeval ts;
  gettimeofday(&ts, NULL);
  InitTime=(uint64_t)ts.tv_sec * 1000000 + (uint64_t)ts.tv_usec;
}
uint64_t GetTime() {
  struct timeval ts;
  gettimeofday(&ts, NULL);
  return (uint64_t)ts.tv_sec * 1000000 + (uint64_t)ts.tv_usec;
}
//init
void init_log(const char *log_file) {
  log_fp = stdout;
  if (log_file != NULL) {
    FILE *fp = fopen(log_file, "w");
    Assert(fp, "Can not open '%s'", log_file);
    log_fp = fp;
  }
  Log("Log is written to %s", log_file ? log_file : "stdout");
}

void assert_fail_msg() {
  isa_reg_display();
}
void sdb_set_batch_mode() {
  is_batch_mode = true;
}
static int parse_args(int argc, char *argv[]) {
  const struct option table[] = {
    {"batch"    , no_argument      , NULL, 'b'},
    {"log"      , required_argument, NULL, 'l'},
    {"diff"     , required_argument, NULL, 'd'},
    {"port"     , required_argument, NULL, 'p'},
    {"help"     , no_argument      , NULL, 'h'},
    {"ftrace"   , required_argument, NULL, 'f'},
    {0          , 0                , NULL,  0 },
  };
  int o;
  while ( (o = getopt_long(argc, argv, "-bhl:d:p:f:", table, NULL)) != -1) {
    switch (o) {
      case 'b':  sdb_set_batch_mode();break;
      case 'p':  break;
      case 'l': log_file = optarg; break;
      case 'd': diff_so_file = optarg; break;
      case 'f': elf_file = optarg; break;
      case 1: img_file = optarg; return 0;
      default:
        printf("Usage: %s [OPTION...] IMAGE [args]\n\n", argv[0]);
        printf("\t-b,--batch              run with batch mode\n");
        printf("\t-l,--log=FILE           output log to FILE\n");
        printf("\t-d,--diff=REF_SO        run DiffTest with reference REF_SO\n");
        printf("\t-p,--port=PORT          run DiffTest with port PORT\n");
        printf("\t-f,--ftrace=FILE        Ftrace file in\n");
        printf("\n");
        exit(0);
    }
  }
  return 0;
}

#ifdef CONFIG_FTRACE
static long load_elf() {
  
  if (elf_file == NULL) {
    Log("No Func_Table is given.");
    return 4096;
  }

  FILE *fp = fopen(elf_file, "r");
  Assert(fp, "Can not open '%s'", elf_file);

  fseek(fp, 0, SEEK_END);
  long size = ftell(fp);
  Log("The Func_table is %s, size = %ld", elf_file, size);
  fseek(fp, 0, SEEK_SET);
  int i=0;
  //decode func name and pc
  while(fgets(Func[i],CONFIG_FUNCLINESIZE,fp)!=NULL){
    Assert(strlen(Func[i])<CONFIG_FUNCLINESIZE,"Func_line is too big");
    for(int j=0;j<8;j++){
      Func_pc[i][j]=Func[i][j];
    }
    Func_pc[i][8]='\0';
    for(int k=32;k<CONFIG_FUNCLINESIZE;k++){
      if(Func[i][k]=='\n'){
        Func[i][k]='\0';
      }
      Func_name[i][k-32]=Func[i][k];
    }
    Assert(i<CONFIG_FUNCNUM,"Func's number is too big");
    i++;
  }
  fclose(fp);
  return size;
}
void ftrace(uint32_t inst,uint32_t snpc){
  int i;
  int j=0;
  char pc[9];
  uint32_t imm_j=SEXT(((BITS(inst,31,31)<<18)|(BITS(inst,19,12)<<11)|
          (BITS(inst,20,20)<<10)|(BITS(inst,30,21)))<<1,20);
  uint32_t imm_i = SEXT(BITS(inst, 31, 20), 12);
  //printf("inst:%x pc:%08x inst:%08x \n",BITS(inst,6,0),snpc,inst);
  if(BITS(inst,6,0)==0b1101111){
    sprintf(pc,"%08x",snpc+imm_j);
    //printf("pc:%s\n",pc);
    for(i=0;i<CONFIG_FUNCNUM;i++){
      if(strcmp(pc,Func_pc[i])==0){
        printf("%08x: ",snpc);
        for(j=0;j<Depth;j++){
          printf(" ");
        }

        printf("jal call [%s@0x%s]\n",Func_name[i],pc);
        Lv[Depth].FuncDepth=snpc+4;
        strcpy(Lv[Depth].FuncName,Func_name[i]);
        Depth++;
      }
    }
  }
  else if(BITS(inst,6,0)==0b1100111){
    if(BITS(inst, 11, 7)!=0){
      sprintf(pc,"%08x",((top->rootp->top__DOT__DISP__DOT__Regfiles__DOT__rf[(BITS(inst,19,15))]+imm_i)&(~1)));
      for(i=0;i<CONFIG_FUNCNUM;i++){
        if(strcmp(pc,Func_pc[i])==0){

          printf("%08x: ",snpc);

          for(j=0;j<Depth;j++){
            printf(" ");
          }

          printf(" jalr call [%s@0x%s]\n",Func_name[i],pc);
          Lv[Depth].FuncDepth=snpc+4;
          strcpy(Lv[Depth].FuncName,Func_name[i]);
          // FuncDepth[Depth]=s->dnpc+4;
          Depth++;

        } 
      }
    }
    else {
      if(BITS(inst,19,15)==0b00001){
        sprintf(pc,"%08x",top->rootp->top__DOT__DISP__DOT__Regfiles__DOT__rf[1]);

        for(i=0;i<CONFIG_FUNCNUM;i++){
          if(strcmp(pc,Func_pc[i])==-1){
            break;
          }
        }

        for(RetDepth=Depth;RetDepth>0;RetDepth--){
          //printf("ra:%08x,fun:%08x\n",R(1),FuncDepth[k]);
          if(top->rootp->top__DOT__DISP__DOT__Regfiles__DOT__rf[1]==Lv[RetDepth].FuncDepth){
            break;
          }
        } 
        //printf("RET:%d dep:%d funname:%s\n",RetDepth,Depth,Lv[Depth-1].FuncName);
        for(int h=RetDepth-1;h<Depth;h++){
          
          for(int k=Depth-1;k>=RetDepth;k--){
            printf("%08x: ",snpc);
            for(j=0;j<k-1;j++){
                printf(" ");
            }
            sprintf(pc,"%08x",Lv[k].FuncDepth);
            for(i=0;i<CONFIG_FUNCNUM;i++){
              if(strcmp(pc,Func_pc[i])==-1){
                break;
              }
            }
            printf(" ret [%s]\n",Lv[k].FuncName);
          }
          Depth=RetDepth;
        }       
      }
      else{
        sprintf(pc,"%08x",top->rootp->top__DOT__DISP__DOT__Regfiles__DOT__rf[BITS(inst,19,15)]);
        for(i=0;i<CONFIG_FUNCNUM;i++){
          if(strcmp(pc,Func_pc[i])==0){

            printf("%08x: ",snpc);
            for(j=0;j<Depth;j++){
              printf(" ");
            }
            printf(" jr call [%s@0x%s]\n",Func_name[i],pc);
            Lv[Depth].FuncDepth=snpc+4;
            strcpy(Lv[Depth].FuncName,Func_name[i]);
            Depth++;
          } 
        } 
      }
    }
  }
}
#endif

static long load_img() {
  if (img_file == NULL) {
    printf("No image is given. Use the default build-in image.\n");
    return 4096; // built-in image size
  }

  FILE *fp = fopen(img_file, "rb");
  //assert(fp, "Can not open '%s'", img_file);

  fseek(fp, 0, SEEK_END);
  long size = ftell(fp);

  printf("The image is %s, size = %ld\n", img_file, size);

  fseek(fp, 0, SEEK_SET);
  int ret = fread(mem, size, 1, fp);
  assert(ret == 1);

  fclose(fp);
  return size;
}
void npc_trap(uint32_t inst,uint32_t pc){
  int trap_code=1; 
  if(inst==0x00100073){
    trap_code=cpu.gpr[10];
    Log("npc: %s at " FMT_WORD,
          (trap_code == 0 ? ANSI_FMT("HIT GOOD TRAP", ANSI_FG_GREEN) :
            ANSI_FMT("HIT BAD TRAP", ANSI_FG_RED)),pc);
    #ifdef Performance
    Stat::performance();
    #endif
    statistic();
    #ifdef  CONFIG_WAVE_TRECE
    tfp->close();
    #endif
    // nvboard_quit();
    exit(0);
  }         
}
void CLOCK(){
  for(int i=0;i<2;i++)
  {
    contextp->timeInc(1);
    top->clock = !top->clock;
    top->eval();
    #ifdef  CONFIG_WAVE_TRECE
    tfp->dump(contextp->time());    
    #endif
  }
}
void RESET(int time){
  for(int i=0;i<time;i++)
  {
    contextp->timeInc(1);
    top->clock = !top->clock;
    top->reset = 1;
    top->eval();
    #ifdef  CONFIG_WAVE_TRECE
    tfp->dump(contextp->time());    
    #endif
  }
  top->reset = 0;
}

void test_f(){
  #ifdef  CONFIG_WAVE_TRECE
  Verilated::traceEverOn(true);
  top->trace(tfp, 99);
  tfp->open("obj_dir/Vtop.vcd");
  #endif
  top->clock = 0;
  top->reset = 1;
  RESET(12);
  for(int i=0;i<36;i++){
    CLOCK();
    // top->io_in +=1;
  }
  #ifdef  CONFIG_WAVE_TRECE
  tfp->close();
  #endif
}
void init_npc(int argc, char** argv){
  
  #ifdef  CONFIG_WAVE_TRECE
  Verilated::traceEverOn(true);
  #endif

  parse_args(argc, argv);
  #ifdef CONFIG_ITRACE
    init_log(log_file);
  #endif
  #ifndef CONFIG_ISA_loongarch32r
    IFDEF(CONFIG_ITRACE, init_disasm(
      MUXDEF(CONFIG_ISA_x86,     "i686",
      MUXDEF(CONFIG_ISA_mips32,  "mipsel",
      MUXDEF(CONFIG_ISA_riscv,
        MUXDEF(CONFIG_RV64,      "riscv64",
                                "riscv32"),
                                "bad"))) "-pc-linux-gnu"
    ));
  #endif
  
  long img_size = load_img();
  
  cpu.pc=RESET_VECTOR;
  #ifdef FTRACE
  load_elf();
  #endif
  welcome();
  TimeInit();
  #ifdef NVBOARD
  nvboard_bind_all_pins(top.get());
  nvboard_init();
  #endif
   // Trace 99 levels of hierarchy (or see below)
  #ifdef  CONFIG_WAVE_TRECE
  top->trace(tfp, 99);
  tfp->open("obj_dir/Vtop.vcd");
  #endif
  top->clock = 0;
  top->reset = 1;
  #ifdef  CONFIG_WAVE_TRECE
  tfp->dump(contextp->time());
  #endif

  RESET(12);
  for(int i=0;i<MUXDEF(CONFIG_RVE, 16, 32);i++){
    cpu.gpr[i]=0;
  }

  #ifdef  CONFIG_DIFFTEST
  init_difftest(diff_so_file, img_size, difftest_port);
  #endif  

  if(is_batch_mode){
    npc_run();
  }
}

void itrace_inst(int inst,int pc){
#ifdef CONFIG_ITRACE
  if(valid_debug&&inst!=0){
  char *p = logbuf;
  log_write("%s\n",logbuf);
  p += snprintf(p, sizeof(logbuf), FMT_WORD ":", pc_debug);
  int ilen =sizeof(word_t);
  int i;
  uint8_t *insto = (uint8_t *)&(inst_debug);
  for (i = ilen - 1; i >= 0; i --) {
    p += snprintf(p, 4, " %02x", insto[i]);
  }
  int ilen_max = MUXDEF(CONFIG_ISA_x86, 8, 4);
  int space_len = ilen_max - ilen;
  if (space_len < 0) space_len = 0;
  space_len = space_len * 3 + 1;
  memset(p, ' ', space_len);
  p += space_len;
#ifndef CONFIG_ISA_loongarch32r


  disassemble(p, logbuf + sizeof(logbuf) - p,
      pc_debug, (uint8_t *)&(inst_debug), ilen);
  
#else
  p[0] = '\0'; // the upstream llvm does not support loongarch32r
#endif
  }
#endif
}

//all trace 
void TraceAndDiff(){
  if((top->reset==0)&&(top->clock)){
    #ifdef  FTRACE
    ftrace(inst_debug,pc_debug);
    #endif
    cpu.pc=pc_debug;
    // if(valid_debug){
    //   printf("pc %08x inst %08x\n",pc_debug,inst_debug);
    // }
    #ifdef  CONFIG_DIFFTEST
    difftest_step(pc_debug,pc_debug,valid_debug);
    #endif
    
    npc_trap(inst_debug,pc_debug);
    last_inst=inst_debug;
  }
}

void npc_si(){
  //printf("time->%d\n",contextp->time());
  #ifdef CONFIG_ITRACE
  itrace_inst(inst_debug,pc_debug);
  //printf("pc:%08x inst:%08x\n",pc_debug,inst_debug);
  #endif
  TraceAndDiff();
  #ifdef NVBOARD
  nvboard_update();
  #endif

  start_time=GetTime();
  for(int i=0;i<2;i++){
    contextp->timeInc(1);
    top->clock = !top->clock;
    top->eval();
    #ifdef  CONFIG_WAVE_TRECE
      if(cpu.pc<=0x30000000||cpu.pc>=0x3fffffff){
        if(cpu.pc>=0xa00000060||cpu.pc<0xa00000000)
        {
          tfp->dump(contextp->time());
        }
      }    
    
    #endif   
  }
  //printf("pc:%08x inst:%08x\n",pc_debug,inst_debug);
  end_time=GetTime();
  g_nr_guest_inst++;
  g_timer+=end_time-start_time;
  
}
void npc_run(){
  while (1) {
      npc_si();
  }
  #ifdef  CONFIG_WAVE_TRECE
  tfp->close();
  #endif
}