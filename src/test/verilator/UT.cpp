#include <init.h>
#include <GlobalVarible.h>
void CLOCK();
uint8_t sram[0x1000];

void UT_Init(){
    uint32_t *start = (uint32_t *)sram;
    for(int i=0;i<0x1000/4;i++){

        *start = i;
        
        start++;
    }
}
// void SRAM_Read_Check(int data,int addr){

// }
extern "C" void SRAM_read(int raddr,int *rdata){
    assert(raddr%4==0);

    *rdata = *((uint32_t *)(sram+raddr));
    // printf("addr %x data %x\n",raddr,*rdata);
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