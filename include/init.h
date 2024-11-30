#ifndef __INIT_H__
#define __INIT_H__



#include <Vtop.h>
#include <Vtop__Dpi.h>
#include <verilated_vcd_c.h>
#include <Vtop___024root.h>


extern const std::unique_ptr<VerilatedContext> contextp;
extern const std::unique_ptr<Vtop> top;
extern VerilatedVcdC *tfp;
void init_npc(int argc, char** argv);
void npc_trap(uint32_t inst, uint32_t pc);
void npc_si();
void npc_run();
void isa_reg_display();
#endif