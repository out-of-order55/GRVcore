#ifndef PREF_H
#define PREF_H

namespace Stat{
    void branch_performance();
    void ipc_performance();
    void ifu_performance();
    void idu_performance();
    void exu_performance();
    void mem_performance();
    void icache_performance();
    void performance();
    void wr_file();
    extern uint32_t   ideal_instructions;
    extern uint32_t   executed_instructions; 
}


#endif