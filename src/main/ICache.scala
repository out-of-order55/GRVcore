package Cache
import chisel3._
import chisel3.util._
// import scala.util.matching.Regex
import freechips.rocketchip.amba._
class TagRAM(addrWidth:Int,dataWidth:Int,depth:Int) extends Module{
    val io = IO(new SRAMIO(addrWidth,dataWidth))
    val mem = SyncReadMem(depth, UInt(dataWidth.W))
    io.dataOut := mem.readWrite(io.addr, io.dataIn, io.enable, io.write)
}
class DataRAM(addrWidth:Int,dataWidth:Int,depth:Int) extends Module{
    val io = IO(new SRAMIO(addrWidth,dataWidth))
    val mem = SyncReadMem(depth, UInt(dataWidth.W))
    io.dataOut := mem.readWrite(io.addr, io.dataIn, io.enable, io.write)
}

// class MissUnit(addrWidth: Int, dataWidth: Int,parameter:CacheConfig) extends  Module{
//   val missmsg0      = IO((new MissMsg()))
//   val missmsg1      = IO((new MissMsg()))
//   val res_data      = IO(Output(Vec((parameter.blockBytes/(dataWidth/8) ),UInt((dataWidth).W))))
//   val finish        = IO(Output(Bool))
// }
class ICache(addrWidth: Int, dataWidth: Int,parameter:CacheConfig) extends  Module{

  val readport = Seq.fill(2)(IO(new CacheReadIO(addrWidth,dataWidth,parameter.blockBytes)))
  
  val banknum   = parameter.blockBytes/(dataWidth/8) 
  val res_data  = IO(Output(Vec((banknum),UInt((dataWidth).W))))
  val finish    = IO(Output(Bool()))
  val databank0 = (Seq.fill(banknum)(Module(new DataRAM(addrWidth,dataWidth,parameter.nSets))))
  val databank1 = (Seq.fill(banknum)(Module(new DataRAM(addrWidth,dataWidth,parameter.nSets))))

  val offset_bit = log2Ceil(parameter.blockBytes*8)
  val index_bit = log2Ceil(parameter.nSets)
  val tag_bit = (addrWidth-index_bit-offset_bit)
  val bank_bit = log2Ceil(banknum)
//random
  val rp = RegInit(false.B)
  rp := (!rp) 
// tag0 for way0,tag1 for way1
  val tag0      = (Seq.fill(2)(Module(new TagRAM(addrWidth,tag_bit+1,parameter.nSets))))//write both tag vec,vec0 for port0,vec1 for port1 
  val tag1      = (Seq.fill(2)(Module(new TagRAM(addrWidth,tag_bit+1,parameter.nSets))))//write both tag vec,vec0 for port0,vec1 for port1 
  
  val port = (Seq.fill(2)(Wire(new CacheMsg(tag_bit,index_bit,offset_bit,bank_bit))))
  // val bank_off = Seq.fill(2)(UInt(banknum.W))//point which bank is begin
  val rdata0   = Wire(Vec((banknum),((UInt((dataWidth).W)))))
  val rdata1   = Wire(Vec((banknum),((UInt((dataWidth).W)))))
  val tagv0    = Wire(Vec(2,((UInt((tag_bit+1).W)))))
  val tagv1    = Wire(Vec(2,((UInt((tag_bit+1).W)))))
//pipe1 send addr to RAM
  for (i <- 0 until 2) {
    port(i) := AddressSplitter(tag_bit,index_bit,offset_bit,bank_bit,readport(i),addrWidth)
  }
// access tag
  for(i<- 0 until 2){
    tag0(i).io.enable := port(i).ren
    tag0(i).io.addr   := port(i).index
    tag0(i).io.write:=false.B
    tag0(i).io.dataIn := 0.U
    tagv0(i) := tag0(i).io.dataOut 

    tag1(i).io.enable := port(0).ren
    tag1(i).io.addr   := port(0).index
    tag1(i).io.write:=false.B
    tag1(i).io.dataIn := 0.U
    tagv1(i) := tag1(i).io.dataOut 
  }
//access data
  for(i<- 0 until banknum){
    databank0(i).io.enable := port(0).ren
    databank0(i).io.addr := port(0).index
    databank0(i).io.write:=false.B
    databank0(i).io.dataIn := 0.U
    rdata0(i) := databank0(i).io.dataOut 
    when((port(1).ren)===true.B){
      databank1(i).io.enable := port(1).ren
      databank1(i).io.addr := port(1).index
      databank1(i).io.write:=false.B
      databank1(i).io.dataIn := 0.U
      rdata1(i) := databank1(i).io.dataOut 
    }.otherwise{
      databank1(i).io.enable := false.B
      databank1(i).io.addr := 0.U
      databank1(i).io.write:=false.B
      databank1(i).io.dataIn := 0.U
      rdata1(i) := 0.U 
    }
  }
//reg1
  val port_r1     = (Seq.fill(2)(RegInit(0.U.asTypeOf(new CacheMsg(tag_bit,index_bit,offset_bit,bank_bit)))))
  // val bank_off = Seq.fill(2)(UInt(banknum.W))//point which bank is begin
  // val rdata0_r1   = Seq.fill(banknum)(Reg(UInt((dataWidth).W)))
  // val rdata1_r1   = Seq.fill(banknum)(Reg(UInt((dataWidth).W)))
  // val tagv0_r1    = (Seq.fill(2)(RegInit(0.U((tag_bit+1).W))))
  // val tagv1_r1    = (Seq.fill(2)(RegInit(0.U((tag_bit+1).W))))
  for (i <- 0 until 2) {
    port_r1(i) := port(i)
  }
  
//pipe2 generate hit and miss res
//hit0 for read0
  val hit0    = (port_r1(0).tag===tagv0(0))|
                (port_r1(0).tag===tagv1(0))
  val hit1    = (port_r1(1).tag===tagv0(1))|
                (port_r1(1).tag===tagv1(1))

  val hit       = hit0&&(hit1|(!port_r1(1).ren))
  val bankoff   = port_r1(0).bank
  val hit_data  =Wire(Vec((banknum),((UInt((dataWidth).W)))))
  val miss_addr0 = Wire(UInt(addrWidth.W))
  val miss_addr1 = Wire(UInt(addrWidth.W))
  miss_addr0 := Cat(port_r1(0).tag,(port_r1(0).index))<<offset_bit
  miss_addr1 := Cat(port_r1(1).tag,(port_r1(1).index))<<offset_bit
  //this will generate a big MUX table(has bank_num input) 
  for(i<- 0 until banknum){
    when(i.U === bankoff){
      for(j<- 0 until banknum){
        if(j>=i){
          hit_data(j-i) := rdata0(j)
        }else{
          hit_data(j+banknum-i) := rdata1(j)
        }
        
      }
    }.otherwise{
      hit_data(i):=0.U
    }
  }

//reg2
  val hit0_r2      = Reg(Bool())
  val hit1_r2      = Reg(Bool())
  val hit_r2       = Reg(Bool())
  val hit_data_r2  = Reg(Vec((banknum),((UInt((dataWidth).W)))))
  val miss_addr0_r2 = Reg(UInt(addrWidth.W))
  val miss_addr1_r2 = Reg(UInt(addrWidth.W))
  // val data  = Vec((banknum),(Wire(UInt((dataWidth).W))))
  // val data1  = Vec((banknum),(Reg(UInt((dataWidth).W))))
  // data1 := data
  hit0_r2       :=hit0           
  hit1_r2       :=hit1     
  hit_r2        :=hit      
  hit_data_r2   :=hit_data 
  miss_addr0_r2 :=miss_addr0
  miss_addr1_r2 :=miss_addr1
//pipe3
  //if hit 
  res_data:=Mux(hit_r2,hit_data_r2,rdata0)
  finish := Mux(hit_r2,true.B,false.B)
  //if miss
  
    // for(i)
    // for (i <- 0 until 2) {
    //   if(i==po)
    //   bank(i).io.enable := false.B
    //   bank(i).io.addr := 0.U
    //   bank(i).io.write := true.B
    //   bank(i).io.dataIn := 1.U
    //   readport(i).rdata := bank(i).io.dataOut 
    // }
}