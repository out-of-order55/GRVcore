#include <init.h>
#include <GlobalVarible.h>


uint8_t sram[0x1000];
char *UT_file = "/home/gg/ysyx-workbench/am-kernels/tests/cpu-tests/build/dummy-riscv32-ysyxsoc.bin";
static long UT_load_img() {
    if (UT_file == NULL) {
    printf("No image is given. Use the default build-in image.\n");
    return 4096; // built-in image size
    }

    FILE *fp = fopen(UT_file, "rb");
    //assert(fp, "Can not open '%s'", img_file);

    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);

    Log("The image is %s, size = %ld\n", UT_file, size);

    fseek(fp, 0, SEEK_SET);
    int ret = fread(sram, size, 1, fp);
    assert(ret == 1);

    fclose(fp);
    return size;
}
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
int max_ptr;
void BR_init(){

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
    max_ptr=i;
    // printf("i:%d\n",i);
    assert(i<=1000);
    fclose(file);
}
void UT_Init(){
    Log("UT init....");
    uint32_t *start = (uint32_t *)sram;
    BR_init();
    for(int i=0;i<0x1000/4;i++){
        *start = i;
        start++;
    }
    UT_load_img();
    Log("UT init finish");
}
extern "C" void Br_check(int* s0_pc,int* pc,int* target,int* br_type,int* taken,int* rs1,
                int* rd,int valid3,int valid0,int s0_ptr,int s3_ptr){
    if(valid3){
        if(s3_ptr==max_ptr-1){
            printf("finish br %d \n",s3_ptr);
            #ifdef CONFIG_WAVE_TRECE
                tfp->close();
            #endif
            exit(0);
        }
        *pc      = br_info[s3_ptr].pc     ;
        *target  = br_info[s3_ptr].target ;
        *br_type = br_info[s3_ptr].type   ;
        *taken   = br_info[s3_ptr].taken  ;
        *rs1     = br_info[s3_ptr].rs1    ;
        *rd      = br_info[s3_ptr].rd     ;
        
    }else{
        *pc      =0;
        *target  =0;
        *br_type =0;
        *taken   =0;
        *rs1     =0;
        *rd      =0;
    }
    if(valid0){
        *s0_pc   = br_info[s0_ptr].pc     ;
    }
}
extern "C" void SRAM_read(int raddr,int *rdata){
    assert(raddr%4==0);
    *rdata = *((uint32_t *)(sram+raddr));
}
extern "C" void check(int finish, int ret){
    if(finish==1){
        Log("UT: %s  " ,
            (ret == 0 ? ANSI_FMT("HIT GOOD TRAP", ANSI_FG_GREEN) :
                ANSI_FMT("HIT BAD TRAP", ANSI_FG_RED)));
        // CLOCK();
        #ifdef CONFIG_WAVE_TRECE
            tfp->close();
        #endif
        exit(0);
    }
}