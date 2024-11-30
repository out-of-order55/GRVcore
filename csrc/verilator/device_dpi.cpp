#include <GlobalVarible.h>
// #include <Vtop__Dpi.h>
bool  timer_addr=false;
bool  diff_amend=false;

////////////////////// DEBUG ///////////////////////////
u_char rst_debug   = 0;
word_t pc_debug    = 0;
word_t inst_debug  = 0;
u_char valid_debug = 0;
uint8_t  psram_data[0x8000000];
uint16_t sdram1[4][8192][512];
uint16_t sdram2[4][8192][512];
uint16_t sdram3[4][8192][512];
uint16_t sdram4[4][8192][512];
//////////////////////////////////////////////////////
/////////////////////DEVICE_BASE//////////////////////
#define MROM_BASE 0x20000000
#define LINE_STATE 0x10000005
#define UART_BUF   0x10000000
#define PS2        0x10011000
#define VGA        0X21000000
extern "C" void sdram_model1(uint32_t wen,uint32_t ren,uint32_t wdata,uint32_t bank,uint32_t column,uint32_t row,uint32_t  mask,uint32_t *rdata){
    if(wen){
        if(mask==0){
            //printf("wdata:%08x\n",wdata);
            sdram1[bank][row][column] = wdata;
        }
        if(mask==1){
            uint8_t *addr = ((uint8_t *)&sdram1[bank][row][column]);
            addr++;
            uint8_t data = wdata>>8;
            //printf("data:%08x %08x\n",data,*addr);
            *addr = data;
            //printf("data:%08x %08x\n",data,sdram[bank][row][column]);
        }
        if(mask==2){
            uint8_t *addr = ((uint8_t *)&sdram1[bank][row][column]);
            uint8_t data = wdata;
            //printf("data:%08x\n",data);
            *addr = data;
        }
    }
    if(ren){
        *rdata = sdram1[bank][row][column];
        //printf("clo:%08x rdata:%08x\n",column,*rdata);
    }
}
extern "C" void sdram_model2(uint32_t wen,uint32_t ren,uint32_t wdata,uint32_t bank,uint32_t column,uint32_t row,uint32_t  mask,uint32_t *rdata){
    if(wen){
        if(mask==0){
            //printf("wdata:%08x\n",wdata);
            sdram2[bank][row][column] = wdata;
        }
        if(mask==1){
            uint8_t *addr = ((uint8_t *)&sdram2[bank][row][column]);
            addr++;
            uint8_t data = wdata>>8;
            //printf("data:%08x %08x\n",data,*addr);
            *addr = data;
            //printf("data:%08x %08x\n",data,sdram[bank][row][column]);
        }
        if(mask==2){
            uint8_t *addr = ((uint8_t *)&sdram2[bank][row][column]);
            uint8_t data = wdata;
            //printf("data:%08x\n",data);
            *addr = data;
        }
    }
    if(ren){
        *rdata = sdram2[bank][row][column];
        //printf("clo:%08x rdata:%08x\n",column,*rdata);
    }
}
extern "C" void sdram_model3(uint32_t wen,uint32_t ren,uint32_t wdata,uint32_t bank,uint32_t column,uint32_t row,uint32_t  mask,uint32_t *rdata){
    if(wen){
        if(mask==0){
            //printf("wdata:%08x\n",wdata);
            sdram3[bank][row][column] = wdata;
        }
        if(mask==1){
            uint8_t *addr = ((uint8_t *)&sdram3[bank][row][column]);
            addr++;
            uint8_t data = wdata>>8;
            //printf("data:%08x %08x\n",data,*addr);
            *addr = data;
            //printf("data:%08x %08x\n",data,sdram[bank][row][column]);
        }
        if(mask==2){
            uint8_t *addr = ((uint8_t *)&sdram3[bank][row][column]);
            uint8_t data = wdata;
            //printf("data:%08x\n",data);
            *addr = data;
        }
    }
    if(ren){
        *rdata = sdram3[bank][row][column];
        //printf("clo:%08x rdata:%08x\n",column,*rdata);
    }
}
extern "C" void sdram_model4(uint32_t wen,uint32_t ren,uint32_t wdata,uint32_t bank,uint32_t column,uint32_t row,uint32_t  mask,uint32_t *rdata){
    if(wen){
        if(mask==0){
            //printf("wdata:%08x\n",wdata);
            sdram4[bank][row][column] = wdata;
        }
        if(mask==1){
            uint8_t *addr = ((uint8_t *)&sdram4[bank][row][column]);
            addr++;
            uint8_t data = wdata>>8;
            //printf("data:%08x %08x\n",data,*addr);
            *addr = data;
            //printf("data:%08x %08x\n",data,sdram[bank][row][column]);
        }
        if(mask==2){
            uint8_t *addr = ((uint8_t *)&sdram4[bank][row][column]);
            uint8_t data = wdata;
            //printf("data:%08x\n",data);
            *addr = data;
        }
    }
    if(ren){
        *rdata = sdram4[bank][row][column];
        //printf("clo:%08x rdata:%08x\n",column,*rdata);
    }
}
extern "C" void flash_read(uint32_t addr, uint32_t *data) {
    uint32_t addr_temp = addr-addr%4;
    *data = *(uint32_t *)(&mem[addr_temp]);
}
extern "C" void psram(uint32_t addr,uint32_t wen,uint32_t ren,uint32_t wdata,uint32_t size,uint32_t *rdata){
    //*rdata = ren?(*(uint32_t *)(&psram_data[addr])):0;
    if(ren){
        *rdata = (*(uint32_t *)(&psram_data[addr]));
    }
    if(wen){
        switch (size)
        {
        case 1:
            *(uint8_t *)(&psram_data[addr]) = wdata;
            break;
        case 3:
            *(uint16_t *)(&psram_data[addr]) = wdata;
            break;
        case 15:
            *(uint32_t *)(&psram_data[addr]) = wdata;
            break;
        default:
            assert(0);
            break;
        }
        
        //printf("wdata:%08x\n",*(uint32_t *)(&psram_data[addr]));
    } 
}
extern "C" void mrom_read(uint32_t addr, uint32_t *data) { 
    
    *data = *(uint32_t *)(&mem[addr-MROM_BASE]); 
}
extern "C" void dev_read(int addr,int waddr){
    if(addr==LINE_STATE||addr==UART_BUF){
        diff_amend=true;
    }

    if(addr==PS2){
        //printf("ps2\n");
        diff_amend=true;
    }
    if(addr>=VGA&&addr<=VGA+0x1fffff){
        diff_amend=true;
    }
}
extern "C" void clint_read(int addr,int ren){
    if((addr==CONFIG_TIMER)&&ren){
        diff_amend=true;
    }
    else if((addr==CONFIG_TIMER+4)&&ren){
        diff_amend=true;
    }
}
extern "C" void debug_info(int pc,int inst,int valid,int rst,int wen,int waddr,int wdata){
    rst_debug   = rst;
    pc_debug    = pc;
    inst_debug  = inst;
    valid_debug = valid;
    if((valid==1&&(wen==1))){
        // printf("cpu en %08x addr %d data %08x\n",wen,waddr,wdata);
        cpu.gpr[waddr]=wdata;

    }
}