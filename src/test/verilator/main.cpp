
// #include <Vtop__Dpi.h>
#include <init.h>//must first write it
#include <GlobalVarible.h>
void init_npc(int argc, char** argv);
void sdb_mainloop();
void test_f();
uint32_t last_inst;

int main(int argc, char** argv) {
  Verilated::commandArgs(argc, argv);
  test_f();

}