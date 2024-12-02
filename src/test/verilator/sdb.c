/***************************************************************************************
* Copyright (c) 2014-2022 Zihao Yu, Nanjing University
*
* NEMU is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/
//#include <stdio.h>

#include "sdb.h"
#include <init.h>
#include <GlobalVarible.h>
// word_t vaddr_read(uint32_t addr, int len);
void isa_reg_display();
/* We use the `readline' library to provide more flexibility to read from stdin. */
static char* rl_gets() {
  static char *line_read = NULL;

  if (line_read) {
    free(line_read);
    line_read = NULL;
  }

  line_read = readline("(npc) ");

  if (line_read && *line_read) {
    add_history(line_read);
  }

  return line_read;
}

static int cmd_c(char *args) {
  npc_run();
  return 0;
}


static int cmd_q(char *args) {
  return -1;
}

static int cmd_help(char *args);
static int cmd_si(char *args);
static int cmd_info(char *args);
static int cmd_x(char *args);
static int cmd_d(char *args);
static int cmd_p(char *args);
static int cmd_w(char *args);
static int cmd_de(char *args);
static int cmd_at(char *args);
static int cmd_save(char *args);
static int cmd_load(char *args);


static struct {
  const char *name;
  const char *description;
  int (*handler) (char *);
} cmd_table [] = {
  { "help", "Display information about all supported commands", cmd_help },
  { "c", "Continue the execution of the program", cmd_c },
  { "q", "Exit NEMU", cmd_q },
  { "si","Single step",cmd_si},
  { "info","Print program status",cmd_info},
  { "x","Scan memory",cmd_x},
  {"d","Delate the watchpoint",cmd_d},
  {"p","process the expression",cmd_p},
  {"w","Set watchpoint",cmd_w},
  {"detach","Quit difftest",cmd_de},
  {"attach","Enable difftest",cmd_at},
  {"save","Save the snapshot",cmd_save},
  {"load","Load the snapshot",cmd_load},


  /* TODO: Add more commands */

};

#define NR_CMD ARRLEN(cmd_table)

static int cmd_save(char *args){
  return 1;
}

static int cmd_load(char *args){
  return 1;
}

static int cmd_de(char *args){
  return 1;
}
static int cmd_at(char *args){
  return 1;
}

static int cmd_w(char *args){
 return 0;
}
static int cmd_p(char *args){
  return 0;
}

static int cmd_d(char *args){
  return 1;
}

static int cmd_x(char *args){
  // char *arg = strtok(NULL, " ");
  // int64_t  num=0;
  // uint32_t addr=0;
  // num=atoi(arg);
  // char *b = strtok(NULL," ");
  // sscanf(b, "%x", &addr);
  // for(int i=0;i<num;i++){ 
  //   uint32_t val=vaddr_read(addr+4*i,4);
  //   printf("0x%x:\tVal:%08x\n",addr+4*i,val);
  // }
  return 0;
};

static int cmd_info(char *args){
  char *arg = strtok(NULL, " ");
  if(strcmp(arg,"r")==0){
    isa_reg_display();
  }
  return 0;
}

static int cmd_si(char *args) {
    int64_t  si_num=0;
    char *arg = strtok(NULL, " ");
    if(arg!=NULL){
      si_num=atoi(arg);
      if(si_num<0)
      {
        si_num=0;
        printf("The input should be greater than zero,Please try again\n");
        assert(0);
      }
    }
    else{
      si_num=1;
    }
    for(int i=0;i<si_num;i++){
        printf("%s\n",logbuf);
        npc_si();
        
    }
    return 0;
}
static int cmd_help(char *args) {
  /* extract the first argument */
  char *arg = strtok(NULL, " ");
  int i;

  if (arg == NULL) {
    /* no argument given */
    for (i = 0; i < NR_CMD; i ++) {
      printf("%s - %s\n", cmd_table[i].name, cmd_table[i].description);
    }
  }
  else {
    for (i = 0; i < NR_CMD; i ++) {
      if (strcmp(arg, cmd_table[i].name) == 0) {
        printf("%s - %s\n", cmd_table[i].name, cmd_table[i].description);
        return 0;
      }
    }
    printf("Unknown command '%s'\n", arg);
  }
  return 0;
}


void sdb_mainloop() {

  for (char *str; (str = rl_gets()) != NULL; ) {
    char *str_end = str + strlen(str);

    /* extract the first token as the command */
    char *cmd = strtok(str, " ");
    if (cmd == NULL) { continue; }

    /* treat the remaining string as the arguments,
     * which may need further parsing
     */
    char *args = cmd + strlen(cmd) + 1;
    if (args >= str_end) {
      args = NULL;
    }


    int i;
    for (i = 0; i < NR_CMD; i ++) {
      if (strcmp(cmd, cmd_table[i].name) == 0) {
        if (cmd_table[i].handler(args) <0) { return; }
        break;
      }
    }

    if (i == NR_CMD) { printf("Unknown command '%s'\n", cmd); }
  }
}

