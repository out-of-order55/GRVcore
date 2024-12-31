package grvcore

/* 
当指令提交写入store buffer（类似于小的Cache）
如果写入请求在store buffer找到地址，直接合并，如果没找到地址，直接分配新的表项
 */
