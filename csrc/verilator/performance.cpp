#include <GlobalVarible.h>
#include <init.h>

#include <performance.h>

    uint32_t  branch_c,pred_succ_c;
    uint32_t  ifu_finish;
    uint32_t  load_num,store_num,bjp_num,csr_num,normal_num,mem_num; 
    uint32_t  exu_num;
    uint32_t  mem_fin_num,mem_rnum,mem_wnum,mem_bjp_t,mem_alu_t,mem_agu_t,mem_csr_t,mem_load_t,mem_store_t,
                mem_uart_num,mem_flash_num,mem_sram_num,mem_sdram_num,
                mem_uart_num_t,mem_flash_num_t,mem_sram_num_t,mem_sdram_num_t;
    uint32_t  ifu_access_num,hit_num,miss_num,lsu_hit_num,lsu_miss_num,lsu_access_num;
    void branch_accuracy(int branch_cnt,int pred_succ_cnt){
        branch_c = branch_cnt;
        pred_succ_c=pred_succ_cnt;
    }
    
    void ifu(int ifu_cnt){
        ifu_finish = ifu_cnt;
    }
    
    void idu(int load_cnt,int store_cnt,int bjp_cnt,int csr_cnt,int normal_cnt,int mem_cnt){
        load_num   = load_cnt;  
        store_num  = store_cnt;
        bjp_num    = bjp_cnt;
        csr_num    = csr_cnt;
        normal_num = normal_cnt;
        mem_num    = mem_cnt;
    }
    void exu(int exu_cnt){
        exu_num = exu_cnt;
    }
    void MEM(int mem_fin_cnt,int mem_rcnt,int mem_wcnt,int bjp_cnt_t,
            int alu_cnt_t,int agu_cnt_t,int csr_cnt_t,int load_cnt_t,int store_cnt_t,
            int mem_uart_cnt,int mem_flash_cnt,int mem_sram_cnt,int mem_sdram_cnt,
            int mem_uart_cnt_t,int mem_flash_cnt_t,int mem_sram_cnt_t,int mem_sdram_cnt_t){
        mem_fin_num  = mem_fin_cnt;
        mem_rnum = mem_rcnt;
        mem_wnum = mem_wcnt;
        mem_bjp_t= bjp_cnt_t;
        mem_alu_t= alu_cnt_t;
        mem_agu_t= agu_cnt_t;
        mem_csr_t= csr_cnt_t;
        mem_load_t=load_cnt_t;
        mem_store_t=store_cnt_t;
        mem_uart_num =mem_uart_cnt;
        mem_flash_num=mem_flash_cnt;
        mem_sdram_num=mem_sdram_cnt;
        mem_sram_num =mem_sram_cnt;
        mem_uart_num_t =mem_uart_cnt_t;
        mem_flash_num_t=mem_flash_cnt_t;
        mem_sdram_num_t=mem_sdram_cnt_t;
        mem_sram_num_t =mem_sram_cnt_t;
    }
    void icache( int ifu_access_cnt, int hit_cnt, int miss_cnt){
            ifu_access_num  = ifu_access_cnt;
            hit_num         = hit_cnt;
            miss_num        = miss_cnt;
    }

    void dcache( int lsu_access_cnt, int hit_cnt, int miss_cnt){
            lsu_access_num  = lsu_access_cnt;
            lsu_hit_num         = hit_cnt;
            lsu_miss_num        = miss_cnt;
    }
namespace Stat{
    uint32_t   ideal_instructions=0;
    uint32_t   executed_instructions=0; 
    void icache_performance(){
        printf("%s\t",ANSI_FMT(str(total), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(hit), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(miss), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\n",ANSI_FMT(str(AMAT), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("%d \t\t%d  %.1f%%\t\t%d  %.1f%%\t\t%.2f \t\n",\
                ifu_access_num, \
                hit_num,    100*((float)hit_num/(float)ifu_access_num) ,\
                miss_num,   100*((float)miss_num /(float)ifu_access_num ) ,\
                2+((float)miss_num /(float)ifu_access_num )*10);
    }
    void ifu_performance(){
        printf("%s\t\n",ANSI_FMT(str(IFU inst), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("%d\t\n\n",ifu_finish);
    }
    void idu_performance(){
        uint32_t total =bjp_num+csr_num+normal_num+mem_num;   
        printf("%s\n",ANSI_FMT(str(Inst type), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("%s\t",ANSI_FMT(str(normal), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(load), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(store), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(mem), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(bjp), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\n",ANSI_FMT(str(csr), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("%d %.1f%%\t\t%d %.1f%%\t\t%d %.1f%%\t\t%d %.1f%%\t\t%d %.1f%%\t\t%d %.1f%%\n\n",\
                normal_num, 100* (float)(normal_num)/(float)(total),\
                load_num,   100 *(float)(load_num  )/(float)(total),\
                store_num,  100*(float)(store_num )/(float)(total),\
                mem_num,    100  *(float)(mem_num   )/(float)(total),\
                bjp_num,    100  *(float)(bjp_num   )/(float)(total),\
                csr_num,    100  *(float)(csr_num   )/(float)(total));
    }
    void exu_performance(){
        printf("%s\t\n",ANSI_FMT(str(EXU_total_inst), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("%d\t\n\n",exu_num);
    }
    void mem_performance(){
        uint32_t total_t = mem_bjp_t+mem_agu_t+mem_alu_t+mem_csr_t;
        uint32_t total_dev_t = mem_flash_num_t+mem_uart_num_t+mem_sdram_num_t+mem_sram_num_t;
        printf("%s\t",ANSI_FMT(str(total_mem_inst), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("%s\t",ANSI_FMT(str(store), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\n",ANSI_FMT(str(load), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("%d\t\t%d %.1f%%\t\t%d %.1f%%\t\t\n",\
                mem_fin_num,\
                mem_wnum,100*((float)mem_wnum/(float)mem_fin_num),\
                mem_rnum,100*((float)mem_rnum/(float)mem_fin_num));

        printf("%s\t",ANSI_FMT(str(bjp_cycle), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(alu_cycle), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(mem_cycle), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(load_cycle), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(store_cycle), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\n",ANSI_FMT(str(csr_cycle), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("%d %.1f%% %.1f\t\t%d %.1f%% %.1f\t\t%d %.1f%% %.1f\t\t%d %.1f%% %.1f\t\t%d %.1f%% %.1f\t\t%d %.1f%% %.1f\t\n\n",\
                mem_bjp_t,  100*((float)mem_bjp_t  /(float)total_t  ),(float)mem_bjp_t/(float)(bjp_num+1),\
                mem_alu_t,  100*((float)mem_alu_t  /(float)total_t  ),(float)mem_alu_t/(float)(normal_num+1),\
                mem_agu_t,  100*((float)mem_agu_t  /(float)total_t  ),(float)mem_agu_t/(float)(mem_num+1),\
                mem_load_t, 100*((float)mem_load_t /(float)total_t ),(float)mem_load_t/(float)(load_num+1),\
                mem_store_t,100*((float)mem_store_t/(float)total_t),(float)mem_store_t/(float)(store_num+1),\
                mem_csr_t,  100*((float)mem_csr_t  /(float)total_t  ),(float)mem_csr_t/(float)(csr_num+1));
        printf("%s\t",ANSI_FMT(str(uart), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(flash), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\t",ANSI_FMT(str(sram), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("\t%s\n",ANSI_FMT(str(sdram), ANSI_FG_YELLOW ANSI_BG_RED));
        printf("%d %d %.1f%%\t\t%d %d %.2f%%\t\t%d %d %.1f%%\t\t%d %d %.2f%%\t\n",\
                mem_uart_num,mem_uart_num_t,    100*((float)mem_uart_num_t /(float)total_dev_t ) ,\
                mem_flash_num,mem_flash_num_t,  100*((float)mem_flash_num_t/(float)total_dev_t) ,\
                mem_sram_num,mem_sram_num_t,    100*((float)mem_sram_num_t /(float)total_dev_t ) ,\
                mem_sdram_num,mem_sdram_num_t,  100*((float)mem_sdram_num_t/(float)total_dev_t) );
    }
    void branch_performance(){
        //assert(branch_c!=0);
        printf("Branch prediction accuracy: %d %%\n",100*pred_succ_c/(branch_c+1));
    }
    void ipc_performance(){
        printf("IPC: %f\n",((float)executed_instructions)/((float)ideal_instructions));
    }
    void performance(){
        ipc_performance();
        ifu_performance();
        idu_performance();
        exu_performance();
        mem_performance();
        icache_performance();

    }
}
