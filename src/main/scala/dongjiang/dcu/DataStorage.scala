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
  val array       = Module(new SinglePortSramTemplate(UInt(beatBits.W), sets, way = maskBits, setup = djparam.dcuSetup, latency = djparam.dcuLatency, extraHold = djparam.dcuExtraHold))

//// ----------------------- Reg/Wire declaration --------------------------//
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
  array.io.req.valid          := io.read.valid | io.write.valid
  array.io.req.bits.write     := io.write.valid
  array.io.req.bits.addr      := Mux(io.write.valid, io.write.bits.index, io.read.bits)
  array.io.req.bits.data.foreach(_ := io.write.bits.beat)
  array.io.req.bits.mask.get  := io.write.bits.mask

  io.write.ready  := array.io.req.ready
  io.read.ready   := array.io.req.ready & !io.write.valid

// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------- S2: Receive SRAM Resp ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //

  /*
   * Receive Meta SRAM resp
   */
  valid_s2      := array.io.resp.valid
  resp_s2       := array.io.resp.bits.data(0)

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