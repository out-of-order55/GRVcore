
package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

class B(implicit p: Parameters) extends GRVBundle()(p)
{
    val a = Bool()
    val b = Bool()
    val c = Bool()
}
class A(implicit p: Parameters) extends GRVBundle()(p)
{
    val f1 = new B()
    val f2 = new B()
    val f3 = new B()
}
abstract class Test(implicit p: Parameters) extends GRVModule{
    val io = IO(new Bundle {

        val resp_in = Input(new A)
        val resp = Output(new A)

    })
    io.resp := io.resp_in
}

class M1(implicit p: Parameters) extends Test{

    io.resp.f1.a := true.B
    io.resp.f1.b := true.B
    io.resp.f1.c := true.B
    dontTouch(io.resp.f1)
    // io.resp.f2 := RegNext(io.resp.f1)
    io.resp.f2.a := true.B
    io.resp.f2.b := true.B
    io.resp.f2.c := true.B
    io.resp.f3 := RegNext(io.resp.f2)
}
class M2(implicit p: Parameters) extends Test{
    io.resp.f2.a := false.B
    io.resp.f2.b := false.B
    
    dontTouch(io.resp.f2)
    io.resp.f3 := RegNext(io.resp.f2)
}
class M3(implicit p: Parameters) extends Test{

    io.resp.f3.a := true.B
    io.resp.f3.b := false.B
    io.resp.f3.c := true.B
    dontTouch(io.resp.f3)
}
class Top(implicit p: Parameters) extends GRVModule{
    val io = IO(new Bundle {

        val resp_in = Input(new A)
        val resp = Output(new A)

    })
    val M1= Module(new M1)
    val M2= Module(new M2)
    val M3= Module(new M3)
    M1.io.resp_in := io.resp_in
    M2.io.resp_in := M1.io.resp
    M3.io.resp_in := M2.io.resp
    io.resp := M3.io.resp
}