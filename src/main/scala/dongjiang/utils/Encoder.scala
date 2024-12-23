package dongjiang.utils

import chisel3._
import chisel3.util._
import chisel3.util.RRArbiter
import xs.utils.ResetRRArbiter

object Encoder {
  def RREncoder(in: Seq[Bool]): UInt = {
    val arb = Module(new ResetRRArbiter(UInt(log2Ceil(in.size).W), in.size))
    arb.io.in.zipWithIndex.foreach {
      case(a, i) =>
        a.valid := in(i)
        a.bits  := i.U
    }
    arb.io.out.ready := true.B
    arb.io.out.bits
  }

  // TODO: It can be instantiated as a module
  def StepRREncoder(in: Seq[Bool], enable: Bool): UInt = {
    require(isPow2(in.size)) // TODO: condition !isPow2(in.size)

    val indexReg  = RegInit(0.U(log2Ceil(in.size).W))
    val indexOut  = WireInit(0.U(log2Ceil(in.size).W))
    val inVec     = Wire(Vec(in.size, Bool()))
    inVec.zip(in).foreach { case(a, b) => a := b }

    when(inVec(indexReg)) {
      indexReg    := Mux(enable, indexReg + 1.U, indexReg)
      indexOut    := indexReg
    }.otherwise {
      indexReg    := Mux(in.reduce(_ | _) & enable, Mux(indexOut > indexReg, indexOut, indexReg + 1.U), indexReg)
      indexOut    := PriorityEncoder(in)
    }
    indexOut
  }
}
