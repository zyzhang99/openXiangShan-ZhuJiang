package dongjiang.dcu

import dongjiang._
import dongjiang.chi._
import chisel3.{util, _}
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.sram.SinglePortSramTemplate


class DsWriteBundle(indexBits: Int)(implicit p: Parameters) extends DJBundle {
  val index = UInt(indexBits.W)
  val beat  = UInt(beatBits.W)
  val mask  = UInt(maskBits.W)
}


class DataStorage(sets: Int)(implicit p: Parameters) extends DJModule {
  val indexBits   = log2Ceil(sets)
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val read      = Flipped(Decoupled(UInt(indexBits.W)))
    val write     = Flipped(Decoupled(new DsWriteBundle(indexBits)))
    val resp      = Valid(UInt(beatBits.W))
  })

// --------------------- Modules declaration ------------------------//
  val split       = 4
  val ways        = maskBits/split
  val arrays      = Seq.fill(split) { Module(new SinglePortSramTemplate(UInt(8.W), sets, way = ways, setup = djparam.dcuSetup, latency = djparam.dcuLatency, extraHold = djparam.dcuExtraHold)) }

//// ----------------------- Reg/Wire declaration --------------------------//
  // s1
  val wMaskVec    = Wire(Vec(split, Vec(ways, Bool())))
  val wDataVec    = Wire(Vec(split, Vec(ways, UInt(8.W))))
  // s2
  val valid_s2    = WireInit(false.B)
  val resp_s2     = Wire(UInt(beatBits.W))
  // s3
  val valid_s3_g  = RegInit(false.B)
  val resp_s3_g   = Reg(UInt(beatBits.W))


// ---------------------------------------------------------------------------------------------------------------------- //
// -------------------------------------------------- S1: Read / Write SRAM --------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Read / Write Req SRAM
   */
  wMaskVec.zipWithIndex.foreach { case(m, i) => m.zipWithIndex.foreach { case(m, j) => m := io.write.bits.mask(i * ways + j) } }
  wDataVec.zipWithIndex.foreach { case(d, i) => d.zipWithIndex.foreach { case(d, j) => d := io.write.bits.beat((i * ways + j + 1) * 8 - 1, (i * ways + j) * 8) } }
  arrays.zipWithIndex.foreach {
    case(a, i) =>
      a.io.req.valid          := io.read.valid | (io.write.valid & wMaskVec(i).reduce(_ | _))
      a.io.req.bits.write     := io.write.valid
      a.io.req.bits.addr      := Mux(io.write.valid, io.write.bits.index, io.read.bits)
      a.io.req.bits.mask.get  := wMaskVec(i).asUInt
      a.io.req.bits.data.zip(wDataVec(i)).foreach { case (a, b) => a := b }
  }


  io.write.ready  := arrays.map(_.io.req.ready).reduce(_ & _)
  io.read.ready   := arrays.map(_.io.req.ready).reduce(_ & _) & !io.write.valid

// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------- S2: Receive SRAM Resp ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //

  /*
   * Receive Meta SRAM resp
   */
  valid_s2      := arrays(0).io.resp.valid
  resp_s2       := Cat(arrays.map { case a => Cat(a.io.resp.bits.data.reverse) }.reverse)

  assert(Mux(arrays.map(_.io.resp.valid).reduce(_ | _), arrays.map(_.io.resp.valid).reduce(_ & _), true.B))

// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------------- S3: Output Resp  ------------------------------------------------ //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Receive S2
   */
  valid_s3_g    := valid_s2
  resp_s3_g     := resp_s2

  /*
   * Output Resp
   */
  io.resp.valid := valid_s3_g
  io.resp.bits  := resp_s3_g

}