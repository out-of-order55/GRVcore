#include <init.h>
#include <GlobalVarible.h>
#include <array>
#include <svdpi.h>

char *UT_file = "./image/add-riscv32-nemu.bin";
static long UT_load_img() {
    if (UT_file == NULL) {
        printf("No image is given. Use the default build-in image.\n");
        // fflush(stdout);
        return 4096; // built-in image size
    }

    FILE *fp = fopen(UT_file, "rb");
    //assert(fp, "Can not open '%s'", img_file);

    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);

    Log("The image is %s, size = %ld\n", UT_file, size);

    fseek(fp, 0, SEEK_SET);
    int ret = fread(sram, size, 1, fp);
    // fflush(stdout);
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

long UT_Init(){
    Log("UT init....");
    long size = UT_load_img();
    Log("UT init finish");
    return size;
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
extern "C" void SRAM(int raddr,int *rdata,int ren,int wen,int waddr,int wstrb,int wdata){
    assert(raddr%4==0);
    
    

    int mask = wstrb;
    if(ren){
        *rdata = *((uint32_t *)(sram+raddr));
    }else{
        *rdata = 0;
    }
    if(wen){
        for(int i=0;i<4;i++){
            uint8_t byte = (wdata >> 8*i) & 0xFF;  // 最高位（第3字节）
            if(mask&0x1==1){
                sram[i+waddr]=byte;
            }
            mask = mask>>1;
        }
    }


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
#define coreWidth 2
//lrs1是一个cat（rs1_0，rs1_1），其他同理
uint64_t freelist = (~0x1);
int rat[32];
int find_arr[coreWidth];
void find1(){
    int n=0;
    uint64_t list = freelist;
    for(int i=0;i<64;i++){
        if(n==coreWidth){
            break;
        }
        if((list>>i)&0x1){
            find_arr[n]=i;
            uint64_t mask = (~((uint64_t)1<<i));
            list = list&mask;
            n++;
        }
    }
}

extern "C" void softRename(int reqs,int lrs1s,int lrs2s,int ldsts,int* old_pdsts,int* prs1s,int* prs2s,int* pdsts){
    uint64_t lrs1[coreWidth],lrs2[coreWidth],ldst[coreWidth],
    prs1[coreWidth],prs2[coreWidth],pdst[coreWidth],req[coreWidth],old_pdst[coreWidth];

    for(int i=0;i<coreWidth;i++){
        lrs1[i] = (lrs1s&(0x1f<<(5*i)))>>(5*i);
        lrs2[i] = (lrs2s&(0x1f<<(5*i)))>>(5*i);
        ldst[i] = (ldsts&(0x1f<<(5*i)))>>(5*i);
        req[i]  = (reqs&(1<<i))>>i;
    }
    //req freelist
    find1();
    for(int i=0;i<coreWidth;i++){
        pdst[i] = find_arr[i];
        if(req[i]&&(ldst[i]!=0)){
            uint64_t mask = (~((uint64_t)1<<pdst[i]));
            freelist&=mask;
            printf("alloc ID %d pdst %ld freelist %lx %lx\n",i,pdst[i],freelist,mask);
        }
    }

    //req rat
    for(int i=0;i<coreWidth;i++){
        prs1[i]=rat[lrs1[i]];
        prs2[i]=rat[lrs2[i]];
        old_pdst[i] =rat[ldst[i]];
        for(int j=0;j<i;j++){
            if(ldst[j]==lrs1[i]){
                //printf("rs1 Bypass(%d) (%d->%d)\n",ldst[j],j,i);
                prs1[i]=pdst[j];
            }
            if(ldst[j]==lrs2[i]){
                //printf("rs2 Bypass(%d) (%d->%d)\n",ldst[j],j,i);
                prs2[i]=pdst[j];
            }
            if(ldst[j]==ldst[i]){
                //printf("dst Bypass(%d) (%d->%d)\n",ldst[j],j,i);
                old_pdst[i] = pdst[j];
            }
        }
    }
    //remap rat
    for(int i=0;i<coreWidth;i++){
        bool wen =true;
        for(int j=i+1;j<coreWidth;j++){
            if(ldst[j]==ldst[i]){
                wen=false;
            }
        }
        if(wen&req[i]){
            //printf("write map (%d -> %d)\n",ldst[i],pdst[i]);
            rat[ldst[i]] = pdst[i];
        }
    }
    int res_old_dst,res_pdst,res_prs1,res_prs2=0;
    for(int i=coreWidth-1;i>=0;i--){
        res_old_dst+=old_pdst[i];
        res_old_dst=res_old_dst<<(6*i);
        res_pdst += pdst[i];
        res_pdst=res_pdst<<(6*i);

        res_prs1 += prs1[i];
        res_prs1=res_prs1<<(6*i);
        res_prs2 += prs2[i];
        res_prs2=res_prs2<<(6*i);
    }
    if(reqs){
        for(int i=0;i<coreWidth;i++){
            printf("req %d %d ID %d prs1 %ld prs2 %ld pdst %ld old_pdst %ld\n",req[i],reqs,i,
            prs1[i],prs2[i],pdst[i],old_pdst[i]);
        }
    }
    *old_pdsts=res_old_dst;
    *pdsts=res_pdst;
    *prs1s=res_prs1;
    *prs2s=res_prs2;
}
extern void commit_event();
extern "C" void commit_event(
char commit_wen, 
int commit_addr, 
char commit_valid, 
long long commit_data, 
long long commit_pc,
svBit commit_timeout
){
    commit.commit_wen  = commit_wen  ; 
    commit.commit_addr = commit_addr ; 
    commit.commit_valid  = commit_valid  ; 
    commit.commit_data = commit_data ; 
    commit.commit_pc   = commit_pc   ;
    if(commit_timeout){
        Log("Time Out");
        #ifdef  CONFIG_WAVE_TRECE
        tfp->close();
        #endif
        // nvboard_quit();
        exit(0);
    }
    // printf("Commit Event Triggered:\n");
    // printf("  wen   = %d\n", commit_wen);
    // printf("  addr  = 0x%08x\n", commit_addr);
    // printf("  num   = %d\n", commit_num);
    // printf("  data  = 0x%016lx\n", commit_data);
    // printf("  pc    = 0x%016lx\n", commit_pc);
}