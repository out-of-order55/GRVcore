# include ../Makefile

ifeq ($(VERILATOR_ROOT),)
VERILATOR = verilator
VERILATOR_COVERAGE = verilator_coverage
else
export VERILATOR_ROOT
VERILATOR = $(VERILATOR_ROOT)/bin/verilator
VERILATOR_COVERAGE = $(VERILATOR_ROOT)/bin/verilator_coverage
endif


TOP_NAME=SimTop



YSYXSOC   = /home/gg/ysyx-workbench/ysyxSoC
WORK_DIR  = $(shell pwd)
BUILD_DIR = $(WORK_DIR)/obj_dir
# Turn on creating .d make dependency files
INC_PATH := $(WORK_DIR)/include 
INCLUDES = $(addprefix -I, $(INC_PATH))

NAME    = V$(TOP_NAME)

DEFINE_INC = $(YSYXSOC)/perip/uart16550/rtl 
#$(YSYXSOC)/perip/uart16550/rtl $(YSYXSOC)/perip/spi/rtl 


################################################################
#############################VERILATOR##########################
################################################################
# Generate C++ in executable form
VERILATOR_FLAGS += -cc --exe
# Generate makefile dependencies (not shown as complicates the Makefile)
VERILATOR_FLAGS += -MMD
VERILATOR_FLAGS += --autoflush
VERILATOR_FLAGS += --bbox-unsup
# Optimize
VERILATOR_FLAGS += -x-assign 0
VERILATOR_FLAGS += -x-initial 0
#-x-assign fast
# Warn abount lint issues; may not want this on less solid designs
VERILATOR_FLAGS += -Wall
VERILATOR_FLAGS += --timescale "1ns/1ns"
# Make waveforms
VERILATOR_FLAGS += --trace
VERILATOR_FLAGS += --notiming
# Check SystemVerilog assertions
VERILATOR_FLAGS += --assert
# Generate coverage analysis
#VERILATOR_FLAGS += --coverage
VERILATOR_FLAGS += --clk clock
#forbidden  it
VERILATOR_FLAGS += -Wno-DECLFILENAME
VERILATOR_FLAGS += -Wno-UNUSED
VERILATOR_FLAGS += -Wno-UNDRIVEN
VERILATOR_FLAGS += -Wno-WIDTHTRUNC
VERILATOR_FLAGS += -Wno-ASSIGNDLY
VERILATOR_FLAGS += -Wno-PINCONNECTEMPTY
VERILATOR_FLAGS += -Wno-DEFPARAM
VERILATOR_FLAGS += -Wno-WIDTHEXPAND
VERILATOR_FLAGS += -Wno-SYNCASYNCNET
VERILATOR_FLAGS += -Wno-MODDUP
VERILATOR_FLAGS += -Wno-PINMISSING
VERILATOR_FLAGS += -Wno-EOFNEWLINE
#ignore unused signal
# VERILATOR_FLAGS += +incdir+$(DEFINE_INC) 
VERILATOR_FLAGS += +incdir+$(WORK_DIR)/vsrc 
# VERILATOR_FLAGS += +incdir+$(YSYXSOC)/perip/spi/rtl 
# Input files for Verilator
# VSRCS = $(shell find $(abspath ./vsrc) -maxdepth 1 -name "*.sv" -or -name "*.v") 
# $(shell find $(YSYXSOC)/perip -name "*.v") $(YSYXSOC)/build/ysyxSoCFull.v 
# CSRCS = $(shell find $(abspath ./csrc) -name "*.c"  -or -name "*.cpp")
VERILATOR_FLAGS += -j 8
VERILATOR_INPUT = $(GenerateV) $(shell find $(abspath $(WORK_DIR)/src/test/verilator) -name "*.c"  -or -name "*.cpp")
VERILATOR_INPUT +=  $(shell find $(abspath $(WORK_DIR)/src/test/unit_test) -name "*.v"  -or -name "*.sv")
################################################################
#############################   VCS  ###########################
################################################################



# include difftest.mk
DIFF_REF_SO = $(WORK_DIR)/image/riscv32-nemu-interpreter-so
ARGS_DIFF = --diff=$(DIFF_REF_SO)
# include $(WORK_DIR)/src/test/verilator/filelist.mk
IMG?=
override ARGS ?= --log=$(BUILD_DIR)/npc-log.txt
override ARGS += $(ARGS_DIFF)
override ARGS += -b
BINARY   = $(BUILD_DIR)/$(NAME)
# Compile in Verilator runtime debugging, so +verilator+debug works
######################################################################
VERILOG_FILE ?= $(WORK_DIR)/build
GenerateV ?=$(shell find $(abspath $(VERILOG_FILE))   -name "*.sv")


PRJ = playground


NOOP_HOME  ?= $(WORK_DIR)
export NOOP_HOME


emu: verilog
	@echo "------------------DiffTest------------------------"
	@$(MAKE) -C ./difftest emu DESIGN_DIR=$(WORK_DIR)  WITH_CHISELDB=0 WITH_CONSTANTIN=0  NUM_CORES=1

sim-verilog:
	test -d $(VERILOG_FILE)/rtl||
	cp $(GenerateV) $(VERILOG_FILE)/rtl
clean_v:
	@rm -rf $(VSRCS)
verilog:
	@echo "---------------- GENERATE VERILOG ----------------"
	
	@mkdir -p $(VERILOG_FILE)
	mill  -i -j 8 $(PRJ).runMain Elaborate  --split-verilog --target-dir $(VERILOG_FILE)
# @echo $(GenerateV) $(WORK_DIR)/vsrc
# @if [ -n "$(GenerateV)" ]; then \
# 	echo "Copying $(GenerateV) to $(WORK_DIR)/vsrc"; \
# 	cp -f $(GenerateV) $(WORK_DIR)/vsrc; \
# fi
# @rm -r $(VERILOG_FILE)

debug :
	@echo "---------------- GENERATE VERILOG ----------------"
# @VERILATOR_ROOT
	@mkdir -p $(VERILOG_FILE)/rtl &&mkdir -p $(VERILOG_FILE)/vcd
	@exec > >(tee $(VERILOG_FILE)/debug.log) 2>&1 
	@sh ./wave.sh 2>&1 | tee $(VERILOG_FILE)/debug.log || exit 1

	@gtkwave -r $(WORK_DIR)/config/.gtkwaverc $(WORK_DIR)/build/vcd/Vtop.vcd  -A $(WORK_DIR)/build/myconfig.gtkw
# @rm  -r $(VERILOG_FILE)
test:
	@echo "-------------------- UNIT TEST----------------"
	@echo $(GenerateV)

sim: verilog
	@echo "----------------- VERILATE ----------------"
	$(VERILATOR) $(VERILATOR_FLAGS) --top-module  $(TOP_NAME) \
											$(VERILATOR_INPUT)
	@echo "------------------ BUILD -------------------"
	$(MAKE) -j 16  -C obj_dir -f ../Makefile_obj NAME=$(NAME)	

run:sim 
	@echo $(ARGS)
	@echo "--------------------- RUN -------------------"
	$(BINARY) $(ARGS) $(IMG) +trace
	


###############################VCS############################
VCS_CSRC ?=	 $(WORK_DIR)/src/test/vcs/top.c
VCS_VSRC ?=  $(WORK_DIR)/src/test/vcs/tb_top.v $(GenerateV)  $(shell find $(abspath $(WORK_DIR)/src/test/unit_test) -name "*.v"  -or -name "*.sv")

vcs   :verilog
	@echo "--------------------- VCS -------------------"
# echo $(VCS_VSRC)
	@echo $(VCS_VSRC) > filelist.f
	vcs   -LDFLAGS -Wl,--no-as-needed\
				-f filelist.f  \
				-timescale=1ns/1ps \
				-fsdb  -full64  -R  +vc  +v2k  -sverilog  -debug_all  \
				-P ${LD_LIBRARY_PATH}/novas.tab  ${LD_LIBRARY_PATH}/pli.a  \
				$(VCS_CSRC)\
				|  tee  vcs.log  &
# @rm -rf filelist.f
#-------------------------------------------------------------------------------------------------------
verdi  :
	@echo "-------------------- VERDI -------------------"
	verdi -f filelist.f -ssf tb.fsdb &
###############################################################
show-config:
	$(VERILATOR) -V
perf:
	cd $(YSYX_HOME)/am-kernels/benchmarks/microbench && make ARCH=riscv32e-ysyx_soc mainargs=train run

clean:
	rm -rf  ./build ./obj_dir *~  core  csrc  simv*  vc_hdrs.h  ucli.key  urg* *.log  novas.* *.fsdb* verdiLog  64* DVEfiles *.vpd

