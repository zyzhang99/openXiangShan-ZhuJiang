package zhujiang.device.bridge.tlul

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import xijiang.router.base.DeviceIcnBundle
import xs.utils.PickOneLow
import zhujiang.ZJModule
import zhujiang.chi.{DatOpcode, DataFlit, ReqFlit, RespFlit}
import zhujiang.tilelink.{AFlit, DFlit, TilelinkParams}

class TLULBridge(node: Node, ioDataBits: Int, tagOffset: Int)(implicit p: Parameters) extends ZJModule {
  private val compareTagBits = 16
  require(node.nodeType == NodeType.HI)
  private val tlParams = TilelinkParams(sourceBits = log2Ceil(node.outstanding), dataBits = ioDataBits)

  val icn = IO(new DeviceIcnBundle(node))
  val tl = IO(new Bundle {
    val a = Decoupled(new AFlit(tlParams))
    val d = Flipped(Decoupled(new DFlit(tlParams)))
  })

  private def compareTag(addr0: UInt, addr1: UInt): Bool = {
    addr0(compareTagBits + tagOffset - 1, tagOffset) === addr1(compareTagBits + tagOffset - 1, tagOffset)
  }

  icn.tx.req.get.valid := false.B
  icn.tx.req.get.bits := DontCare

  private val wakeups = Wire(Vec(node.outstanding, Valid(UInt(raw.W))))

  private val icnRspArb = Module(new RRArbiter(icn.tx.resp.get.bits.cloneType, node.outstanding))
  icn.tx.resp.get <> icnRspArb.io.out

  private val tlaArb = Module(new RRArbiter(tl.a.bits.cloneType, node.outstanding))
  tl.a <> tlaArb.io.out

  private val cms = for(idx <- 0 until node.outstanding) yield {
    val cm = Module(new TLULBridgeCtrlMachine(node, tlParams, node.outstanding, ioDataBits, compareTag))
    cm.suggestName(s"cm_$idx")
    cm.io.wakeupIns := wakeups.zipWithIndex.filterNot(_._2 == idx).map(_._1)
    wakeups(idx).valid := cm.io.wakeupOut.valid
    wakeups(idx).bits := cm.io.wakeupOut.bits
    cm.io.idx := idx.U
    icnRspArb.io.in(idx).valid := cm.icn.tx.resp.valid
    icnRspArb.io.in(idx).bits := cm.icn.tx.resp.bits.asTypeOf(icn.tx.resp.get.bits.cloneType)
    cm.icn.tx.resp.ready := icnRspArb.io.in(idx).ready
    tlaArb.io.in(idx) <> cm.tla
    cm
  }

  private val shouldBeWaited = cms.map(cm => cm.io.info.valid && !cm.io.wakeupOut.valid)
  private val cmAddrSeq = cms.map(cm => cm.io.info.bits.addr)
  private val req = icn.rx.req.get.bits.asTypeOf(new ReqFlit)
  private val reqTagMatchVec = shouldBeWaited.zip(cmAddrSeq).map(elm => elm._1 && compareTag(elm._2, req.Addr))
  private val waitNum = PopCount(reqTagMatchVec)

  private val busyEntries = cms.map(_.io.info.valid)
  private val enqCtrl = PickOneLow(busyEntries)

  icn.rx.req.get.ready := enqCtrl.valid
  icn.rx.resp.get.ready := true.B
  icn.rx.data.get.ready := true.B

  for((cm, idx) <- cms.zipWithIndex) {
    cm.icn.rx.req.valid := icn.rx.req.get.valid && enqCtrl.bits(idx)
    cm.icn.rx.req.bits := icn.rx.req.get.bits.asTypeOf(new ReqFlit)
    cm.icn.rx.resp.get.valid := icn.rx.resp.get.valid && icn.rx.resp.get.bits.asTypeOf(new RespFlit).TxnID === idx.U
    cm.icn.rx.resp.get.bits := icn.rx.resp.get.bits.asTypeOf(new RespFlit)
    cm.icn.rx.data.valid := icn.rx.data.get.valid && icn.rx.data.get.bits.asTypeOf(new DataFlit).TxnID === idx.U
    cm.icn.rx.data.bits := icn.rx.data.get.bits.asTypeOf(new DataFlit)

    cm.io.readDataFire := tl.d.fire && tl.d.bits.source === idx.U
    cm.io.readDataLast := true.B
    cm.io.waitNum := waitNum
  }

  private val readDataPipe = Module(new Queue(gen = new DataFlit, entries = 1, pipe = true))
  private val ctrlVec = VecInit(cms.map(_.io.info.bits))
  private val ctrlSel = ctrlVec(tl.d.bits.source(log2Ceil(node.outstanding) - 1, 0))

  readDataPipe.io.enq.valid := tl.d.valid
  tl.d.ready := readDataPipe.io.enq.ready

  readDataPipe.io.enq.bits := DontCare
  readDataPipe.io.enq.bits.Data := Fill(dw / ioDataBits, tl.d.bits.data)
  readDataPipe.io.enq.bits.Opcode := DatOpcode.CompData
  readDataPipe.io.enq.bits.DataID := 0.U
  readDataPipe.io.enq.bits.TxnID := ctrlSel.txnId
  readDataPipe.io.enq.bits.SrcID := 0.U
  readDataPipe.io.enq.bits.TgtID := ctrlSel.srcId
  readDataPipe.io.enq.bits.Resp := "b010".U

  icn.tx.data.get.valid := readDataPipe.io.deq.valid
  icn.tx.data.get.bits := readDataPipe.io.deq.bits.asTypeOf(icn.tx.data.get.bits)
  readDataPipe.io.deq.ready := icn.tx.data.get.ready
}