#!/bin/bash

# 定义服务器的 IP 和 SSH 用户
SERVER_IP="10.201.230.232"
SERVER_USER="gb515897968"
WORK_DIR="/home/gg/GRVCore"
# BULID_FILES=
echo "---------------- SENDING FILES  --------------------"
ssh $SERVER_USER@$SERVER_IP "mkdir -p GRVCore"

rsync -avz  $WORK_DIR/.mill-version $WORK_DIR/build.sc $WORK_DIR/difftest.mk $WORK_DIR/Makefile $WORK_DIR/Makefile_obj $WORK_DIR/src $WORK_DIR/include $WORK_DIR/rocket-chip $SERVER_USER@$SERVER_IP:/home/gb515897968/GRVCore
echo "-------------------WORKING--------------------------"
ssh $SERVER_USER@$SERVER_IP "cd /home/gb515897968/GRVCore&&make clean&& time make run"
echo "---------------RECIEVING MSG------------------------"
rsync -avz $SERVER_USER@$SERVER_IP:/home/gb515897968/GRVCore/obj_dir/Vtop.vcd $WORK_DIR/build
rsync -avz $SERVER_USER@$SERVER_IP:/home/gb515897968/GRVCore/build/ $WORK_DIR/build
echo "---------------FINISH WORKING------------------------"