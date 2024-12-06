#include <GlobalVarible.h>
uint8_t sram[0x4000];

extern "C" void SRAM_read( int raddr,int *rdata){
    *rdata = 1;
}