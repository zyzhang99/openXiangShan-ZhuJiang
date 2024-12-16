package zhujiang.device.dma

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import chisel3.util.RRArbiter
import xs.utils.ResetRRArbiter
import zhujiang._
import zhujiang.chi._
import zhujiang.axi._

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
}

object BurstMode {
  val width        = 2
  val Fix          = "b00".U
  val Incr         = "b01".U
  val Wrap         = "b10".U
  val Reserve      = "b11".U
}
object AXIRState {
  val width = 2
  val Free  = "b00".U
  val Send  = "b01".U
  val Comp  = "b10".U
}
class AXIREntry(implicit p: Parameters) extends ZJBundle {
    val state           = UInt(AXIRState.width.W)
    val addr            = UInt(raw.W)
    val burst           = UInt(BurstMode.width.W)
    val len             = UInt(8.W)
    val size            = UInt(3.W)
    val arid            = UInt(8.W)
    val byteMask        = UInt(9.W)
    val nid             = UInt(log2Ceil(zjParams.dmaParams.axiEntrySize).W)
    val sendReqNum      = UInt(6.W)
    val sendDatNum      = UInt(6.W)  

}

class CHIREntry(implicit p : Parameters) extends ZJBundle {
  val areid          = UInt(log2Ceil(zjParams.dmaParams.axiEntrySize).W)
  val state          = UInt(CHIRState.width.W)
  val num            = UInt(6.W)
  val last           = Bool()
  val next           = UInt((log2Ceil(zjParams.dmaParams.chiEntrySize)).W)
  val full           = Bool()
  val homeNid        = UInt(niw.W)
  val dbid           = UInt(12.W)
  val haveRecReceipt = Bool()
  val haveRecData1   = Bool()
  val haveRecData2   = Bool()
}


object CHIRState {
  val width        = 2
  val Free         = "b00".U
  val Wait         = "b01".U
  val SendData     = "b10".U
  val Comp         = "b11".U
}

class IDBundle(implicit p : Parameters) extends ZJBundle {
  val areid      = UInt(log2Ceil(zjParams.dmaParams.axiEntrySize).W)
  val rid        = UInt(zjParams.dmaParams.idBits.W)
  val last       = Bool()
}

class SRAMSelector(implicit p: Parameters) extends ZJModule {
  private val dmaParams = zjParams.dmaParams
  val io = IO(new Bundle() {
    val idle = Input(Vec(dmaParams.chiEntrySize, Bool()))
    val idleNum = Output(UInt((log2Ceil(dmaParams.chiEntrySize) + 1).W))
    val out0 = UInt(log2Ceil(dmaParams.chiEntrySize).W)
    val out1 = UInt(log2Ceil(dmaParams.chiEntrySize).W)
  })
  io.idleNum := PopCount(io.idle)
  io.out0    := PriorityEncoder(io.idle)
  val idle1   = WireInit(io.idle)
  idle1(io.out0) := false.B
  io.out1    := PriorityEncoder(idle1)
}

object CHIWState {
  val width        = 3
  val Free         = "b000".U
  val SendWrReq    = "b001".U
  val WaitDBID     = "b010".U
  val SendWrData   = "b011".U
  val WaitDataSend = "b100".U
  val SendCompAck  = "b101".U
}

object AXIWState{
  val width       = 2
  val Free        = "b00".U
  val ReceiveData = "b01".U
  val Comp        = "b10".U
}

class AXIWEntry(implicit p: Parameters) extends ZJBundle {
    val addr            = UInt(raw.W)
    val unalign         = Bool()
    val burst           = UInt(BurstMode.width.W)
    val awid            = UInt(8.W)
    val byteMask        = UInt(9.W)
    val state           = UInt(AXIWState.width.W)
    val nid             = UInt(log2Ceil(zjParams.dmaParams.axiEntrySize).W)
}

class CHIWEntry(implicit p: Parameters) extends ZJBundle {
  val areid        = UInt(log2Ceil(zjParams.dmaParams.axiEntrySize).W)
  val dbid         = UInt(12.W)
  val last         = Bool()
  val full         = Bool()
  val sendReqOrder = UInt(log2Ceil(zjParams.dmaParams.chiEntrySize).W)
  val state        = UInt(CHIWState.width.W)
  val tgtID        = UInt(niw.W)
  val sendFull     = Bool()
  val mmioReq      = Bool()
  val sendAckOrder = UInt(log2Ceil(zjParams.dmaParams.chiEntrySize).W)
  val haveRecComp  = Bool()
}
