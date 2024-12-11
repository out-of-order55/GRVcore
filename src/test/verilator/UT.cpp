#include <init.h>
#include <GlobalVarible.h>


uint8_t sram[0x1000];
typedef struct Br_info
{
    int pc      ;
    int target  ;
    int type    ;
    int taken   ;
    int rs1     ;
    int rd      ;
}Br_info;

Br_info br_info[1000];
int br_ptr;
void BR_init(){
    br_ptr=0;
    FILE *file = fopen("/home/gg/GRVCore/br_trace.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
    }
    char line[256];
    int i=0;
    while (fgets(line, sizeof(line), file)) {
        unsigned int num1, num2;
        int num3, num4, num5, num6;
        if (sscanf(line, "%x %x %d %d %d %d", &num1, &num2, &num3, &num4, &num5, &num6) == 6) {
            br_info[i].pc     =  num1;
            br_info[i].target =  num2;
            br_info[i].type   =  num3;
            br_info[i].taken  =  num4;
            br_info[i].rs1    =  num5;
            br_info[i].rd     =  num6;
        } else {
            printf("Failed to parse line: %s", line);
            assert(0);
        }
        i++;
    }
    assert(i==1000);
    fclose(file);
}
void UT_Init(){
    uint32_t *start = (uint32_t *)sram;
    BR_init();
    for(int i=0;i<0x1000/4;i++){
        *start = i;
        start++;
    }
}
extern "C" void Br_check(int* pc,int* target,int* type,int* taken,int* rs1,int* rd,int valid){
    if(valid){
        *pc     := br_info[i].pc     ;
        *target := br_info[i].target ;
        *type   := br_info[i].type   ;
        *taken  := br_info[i].taken  ;
        *rs1    := br_info[i].rs1    ;
        *rd     := br_info[i].rd     ;
        br_ptr++;
    }
}
extern "C" void SRAM_read(int raddr,int *rdata){
    assert(raddr%4==0);
    *rdata = *((uint32_t *)(sram+raddr));
}
extern "C" void check(int finish, int ret){
    if(finish==1){
        Log("npc: %s  " ,
            (ret == 0 ? ANSI_FMT("HIT GOOD TRAP", ANSI_FG_GREEN) :
                ANSI_FMT("HIT BAD TRAP", ANSI_FG_RED)));
        // CLOCK();
        #ifdef CONFIG_WAVE_TRECE
            tfp->close();
        #endif
        exit(0);
    }
}