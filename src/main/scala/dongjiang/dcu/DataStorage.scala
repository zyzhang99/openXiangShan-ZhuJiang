package dongjiang.dcu

import dongjiang._
import dongjiang.chi._
import chisel3.{util, _}
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.sram.SinglePortSramTemplate


class DsWriteBundle(indexBits: Int)(implicit p: Parameters) extends DJBundle {
  val index = UInt(indexBits.W)
  val data  = UInt(dataBits.W)
  val mask  = UInt(maskBits.W)
}


class DataStorage(sets: Int)(implicit p: Parameters) extends DJModule {
  val indexBits   = log2Ceil(sets)
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val read      = Flipped(Decoupled(UInt(indexBits.W)))
    val write     = Flipped(Decoupled(new DsWriteBundle(indexBits)))
    val resp      = Valid(UInt(dataBits.W))
  })

// --------------------- Modules declaration ------------------------//
  val arrays      = Seq.fill(nrBeat) { Module(new SinglePortSramTemplate(UInt(beatBits.W), sets, way = maskBits, setup = djparam.dcuSetup, latency = djparam.dcuLatency, extraHold = djparam.dcuExtraHold)) }

//// ----------------------- Reg/Wire declaration --------------------------//
  // s2
  val valid_s2    = WireInit(false.B)
  val resp_s2     = Wire(UInt(dataBits.W))
  // s3
  val valid_s3_g  = RegInit(false.B)
  val resp_s3_g   = Reg(UInt(dataBits.W))


// ---------------------------------------------------------------------------------------------------------------------- //
// -------------------------------------------------- S1: Read / Write SRAM --------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Read / Write Req SRAM
   */
  arrays.zipWithIndex.foreach {
    case(a, i) =>
      // ren
      a.io.req.valid          := io.read.valid | io.write.valid
      a.io.req.bits.write     := io.write.valid
      a.io.req.bits.addr      := Mux(io.write.valid, io.write.bits.index, io.read.bits)
      a.io.req.bits.data.foreach(_ := io.write.bits.data(beatBits * (i + 1) - 1, beatBits * i))
      a.io.req.bits.mask.get  := io.write.bits.mask
  }
  io.write.ready  := arrays(0).io.req.ready
  io.read.ready   := arrays(0).io.req.ready & !io.write.valid

  assert(Mux(arrays(0).io.req.ready, arrays.map(_.io.req.ready).reduce(_ & _), true.B))

// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------- S2: Receive SRAM Resp ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //

  /*
   * Receive Meta SRAM resp
   */
  valid_s2      := arrays(0).io.resp.valid
  resp_s2       := Cat(arrays.map(_.io.resp.bits.data(0)).reverse)

  assert(Mux(arrays(0).io.resp.valid, arrays.map(_.io.resp.valid).reduce(_ & _), true.B))

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