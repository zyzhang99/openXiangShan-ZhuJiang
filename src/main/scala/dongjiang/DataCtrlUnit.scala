package dongjiang.dcu

import zhujiang.chi._
import xijiang.Node
import dongjiang._
import dongjiang.chi._
import zhujiang.chi.ReqOpcode._
import zhujiang.chi.RspOpcode._
import zhujiang.chi.DatOpcode._
import zhujiang.chi._
import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import xijiang.router.base.DeviceIcnBundle
import xs.utils.sram._
import dongjiang.utils.FastArb._

/*
 * Read Req:
 * Free ----> ReadSram ----> Reading
 */
object DCURState {
  val width             = 2
  val Free              = "b00".U
  val ReadSram          = "b01".U
  val Reading           = "b10".U
}


class DCUREntry(implicit p: Parameters) extends DJBundle {
  val state             = UInt(DCURState.width.W)
  val dsIndex           = UInt(dsIndexBits.W)
  val dsBank            = UInt(dsBankBits.W)
  val srcID             = UInt(fullNodeIdBits.W)
  val txnID             = UInt(chiTxnIdBits.W)
  val returnNID         = UInt(fullNodeIdBits.W)
  val returnTxnID       = UInt(chiTxnIdBits.W)
  val resp              = UInt(ChiResp.width.W)
}


/*
 * Write Req:
 * Free ----> SendDBIDResp ----> WaitData -----> WriteSram -----> Writting -----> (SendComp) -----> Free
 *                                                           ^
 *                                                    WaitReplReadDone
 */
object DCUWState {
  val width = 3
  // commom
  val Free              = "b000".U
  val SendDBIDResp      = "b001".U
  val WaitData          = "b010".U
  val WriteSram         = "b011".U // Send Write Req To DataStorage And Send Comp To Src
  val Writting          = "b100".U // Write Data To DataStorage
  val SendComp          = "b101".U
}

class DCUWEntry(implicit p: Parameters) extends DJBundle {
  val state             = UInt(DCUWState.width.W)
  val replRState        = UInt(DCURState.width.W)
  val data              = Vec(nrBeat, Valid(UInt(beatBits.W)))
  val dsIndex           = UInt(dsIndexBits.W)
  val dsBank            = UInt(dsBankBits.W)
  val srcID             = UInt(fullNodeIdBits.W)
  val txnID             = UInt(chiTxnIdBits.W)
  val returnTxnID       = UInt(chiTxnIdBits.W)

  def canWrite          = replRState === DCURState.Free
  def isLast            = PopCount(data.map(_.valid)) === (nrBeat - 1).U
}



// TODO: DMT
// TODO: DWT
/*
 * DCUs do not have sorting capabilities and must use the DWT transfer structure to sort by using Comp
 */
@instantiable
class DataCtrlUnit(node: Node, nrIntf: Int = 1)(implicit p: Parameters) extends DJRawModule
  with ImplicitClock with ImplicitReset {
  // ------------------------------------------ IO declaration --------------------------------------------- //
  @public val io = IO(new Bundle {
    val sn = Vec(nrIntf, new DeviceIcnBundle(node))
  })
  @public val reset = IO(Input(AsyncReset()))
  @public val clock = IO(Input(Clock()))
  val implicitClock = clock
  val implicitReset = reset

  if(p(DebugOptionsKey).EnableDebug) {
    dontTouch(io)
  }

  require(node.splitFlit)
  require(1 <= nrIntf & nrIntf <= 2)

  io <> DontCare

// ----------------------------------------- Reg and Wire declaration ------------------------------------ //
  // CHI
  val rxReq                 = Wire(new DecoupledIO(new ReqFlit))
  val rxDat                 = Wire(new DecoupledIO(new DataFlit))
  val txRsp                 = WireInit(0.U.asTypeOf(Decoupled(new RespFlit)))
  val txDat                 = WireInit(0.U.asTypeOf(Decoupled(new DataFlit)))
  // TODO
  io.sn(0).rx.req.get       <> rxReq
  io.sn(0).rx.data.get      <> rxDat
  io.sn(0).tx.resp.get      <> txRsp
  io.sn(0).tx.data.get      <> txDat


  // ReqBuf
  val wBufRegVec            = RegInit(VecInit(Seq.fill(djparam.nrDCUWBuf) { 0.U.asTypeOf(new DCUWEntry) }))
  val rBufRegVec            = RegInit(VecInit(Seq.fill(djparam.nrDCURBuf) { 0.U.asTypeOf(new DCUREntry) }))

  // sram read vec
  val sramRReadyVec         = Wire(Vec(djparam.nrDSBank, Bool()))
  val sramWReadyVec         = Wire(Vec(djparam.nrDSBank, Bool()))

  // DataStorage
  val ds                    = Seq.fill(djparam.nrDSBank) { Module(new DataStorage(sets = nrDSEntry)) }
  ds.zipWithIndex.foreach { case(d, i) => d.io.id := i.U }

  // ChiRespQueue
  val rRespReg              = RegInit(0.U.asTypeOf(Valid(new DataFlit)))
  val rRespCanGo            = Wire(Bool())
  val rRespQ                = Module(new Queue(new DataFlit(), entries = djparam.nrDCURespQ - 1, flow = false, pipe = true))
  val rDatQ                 = Module(new Queue(Vec(nrBeat, UInt(beatBits.W)), entries = djparam.nrDCURespQ, flow = false, pipe = true))
  val sendBeatNumReg        = RegInit(0.U(beatNumBits.W))
  val respVec               = Wire(Vec(djparam.nrDSBank, Valid(UInt(dataBits.W))))


// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------ S0: Receive Req From CHI -------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Receive Req
   */
  val wBufFreeVec           = wBufRegVec.map(_.state === DCUWState.Free)
  val rBufFreeVec           = rBufRegVec.map(_.state === DCURState.Free)
  val selRecWID             = PriorityEncoder(wBufFreeVec)
  val selRecRID             = PriorityEncoder(rBufFreeVec)
  val reqIsW                = isWriteX(rxReq.bits.Opcode)
  val reqIsRepl             = isReplace(rxReq.bits.Opcode)
  when(reqIsW | reqIsRepl) {
    rxReq.ready             := wBufFreeVec.reduce(_ | _)
  }.otherwise {
    rxReq.ready             := rBufFreeVec.reduce(_ | _)
  }


  /*
   * Send DBID Or Comp To Src
   */
  val wBufSCompVec          = wBufRegVec.map { case w => w.state === DCUWState.Writting | w.state === DCUWState.SendComp}
  val wBufSDBIDVec          = wBufRegVec.map(_.state === DCUWState.SendDBIDResp)
  val selSCompID            = PriorityEncoder(wBufSCompVec)
  val selSDBID              = PriorityEncoder(wBufSDBIDVec)

  txRsp.valid               := wBufSDBIDVec.reduce(_ | _) | wBufSCompVec.reduce(_ | _)
  txRsp.bits.Opcode         := Mux(wBufSCompVec.reduce(_ | _), Comp,                         DBIDResp)
  txRsp.bits.DBID           := Mux(wBufSCompVec.reduce(_ | _), selSCompID,                   selSDBID)
  txRsp.bits.TgtID          := Mux(wBufSCompVec.reduce(_ | _), wBufRegVec(selSCompID).srcID, wBufRegVec(selSDBID).srcID)
  txRsp.bits.TxnID          := Mux(wBufSCompVec.reduce(_ | _), wBufRegVec(selSCompID).txnID, wBufRegVec(selSDBID).txnID)


  /*
      * Select Buf Send Early Req To SRAM
      */
  sramRReadyVec             := ds.map(_.io.earlyRReq.ready)
  sramWReadyVec             := ds.map(_.io.earlyWReq.ready)

  val willSendRVec          = rBufRegVec.map { case r => r.state === DCURState.ReadSram      & sramRReadyVec(r.dsBank) & (!rRespReg.valid | rRespCanGo) }
  val willSendReplVec       = wBufRegVec.map { case r => r.replRState === DCURState.ReadSram & sramRReadyVec(r.dsBank) & (!rRespReg.valid | rRespCanGo) & !willSendRVec.reduce(_ | _) }
  val willSendWVec          = wBufRegVec.map { case w => w.state === DCUWState.WriteSram     & w.canWrite & sramWReadyVec(w.dsBank) }

  val earlyWID              = PriorityEncoder(willSendWVec)
  val earlyRID              = PriorityEncoder(willSendRVec)
  val earlyReplID           = PriorityEncoder(willSendReplVec)
  val earlyRepl             = !willSendRVec.reduce(_ | _) & willSendReplVec.reduce(_ | _)

  ds.zipWithIndex.foreach {
    case(d, i) =>
      d.io.earlyRReq.valid := (willSendRVec.reduce(_ | _) | willSendReplVec.reduce(_ | _)) & rBufRegVec(earlyRID).dsBank === i.U
      d.io.earlyWReq.valid := willSendWVec.reduce(_ | _)                                   & wBufRegVec(earlyWID).dsBank === i.U
  }


  /*
    * Get Read CHI Resp
    */
  val earlyRValid           = ds.map(_.io.earlyRReq.fire).reduce(_ | _)
  rRespReg.valid            := Mux(earlyRValid, true.B, rRespReg.valid & !rRespCanGo)
  rRespCanGo                := rRespQ.io.enq.ready
  when(earlyRValid & rRespCanGo) {
    rRespReg.bits.Opcode    := Mux(earlyRepl, NonCopyBackWriteData,                 CompData)
    rRespReg.bits.TgtID     := Mux(earlyRepl, ddrcNodeId.U,                         rBufRegVec(earlyRID).returnNID)
    rRespReg.bits.TxnID     := Mux(earlyRepl, wBufRegVec(earlyReplID).returnTxnID,  rBufRegVec(earlyRID).returnTxnID)
    rRespReg.bits.Resp      := Mux(earlyRepl, 0.U,                                  rBufRegVec(earlyRID).resp)
    rRespReg.bits.HomeNID   := Mux(earlyRepl, 0.U,                                  rBufRegVec(earlyRID).srcID)
    rRespReg.bits.DBID      := Mux(earlyRepl, 0.U,                                  rBufRegVec(earlyRID).txnID)
  }


  /*
    * Read Req Buf
    */
  rBufRegVec.zipWithIndex.foreach {
    case (r, i) =>
      switch(r.state) {
        is(DCURState.Free) {
          val hit       = rxReq.fire & !reqIsW & !reqIsRepl & selRecRID === i.U
          when(hit) {
            r.state     := DCURState.ReadSram
            r.dsIndex   := parseDCUAddr(rxReq.bits.Addr)._1
            r.dsBank    := parseDCUAddr(rxReq.bits.Addr)._2
            r.srcID     := rxReq.bits.SrcID
            r.txnID     := rxReq.bits.TxnID
            r.returnNID := rxReq.bits.ReturnNID
            r.returnTxnID := rxReq.bits.ReturnTxnID
            r.resp      := rxReq.bits.MemAttr(ChiResp.width - 1, 0)
          }
        }
        is(DCURState.ReadSram) {
          val hit       = ds.map(_.io.earlyRReq.fire).reduce(_ | _) & earlyRID === i.U & !earlyRepl
          when(hit) {
              r.state   := DCURState.Reading
          }
        }
        is(DCURState.Reading) {
          r             := 0.U.asTypeOf(r)
          r.state       := DCURState.Free
        }
      }
  }


  /*
    * Write Req Buf Write State
    */
  wBufRegVec.zipWithIndex.foreach {
    case(w, i) =>
      switch(w.state) {
        is(DCUWState.Free) {
          val hit       = rxReq.fire & (reqIsW | reqIsRepl) & selRecWID === i.U
          when(hit) {
            w.state     := DCUWState.SendDBIDResp
            w.dsIndex   := parseDCUAddr(rxReq.bits.Addr)._1
            w.dsBank    := parseDCUAddr(rxReq.bits.Addr)._2
            w.srcID     := rxReq.bits.SrcID
            w.txnID     := rxReq.bits.TxnID
          }
        }
        is(DCUWState.SendDBIDResp) {
          val hit       = txRsp.fire & txRsp.bits.Opcode === DBIDResp & selSDBID === i.U
          when(hit) {
            w.state     := DCUWState.WaitData
          }
        }
        is(DCUWState.WaitData) {
          val hit       = rxDat.fire & rxDat.bits.TxnID === i.U
          when(hit) {
            w.state     := Mux(hit & w.isLast, DCUWState.WriteSram, w.state)
            w.data(toBeatNum(rxDat.bits.DataID)).valid  := true.B
            w.data(toBeatNum(rxDat.bits.DataID)).bits   := rxDat.bits.Data
          }
        }
        is(DCUWState.WriteSram) {
          val hit       = ds.map(_.io.earlyWReq.fire).reduce(_ | _) & earlyWID === i.U
          when(hit) {
            w.state     := DCUWState.Writting
          }
        }
        is(DCUWState.Writting) {
          val hit       = txRsp.fire & txRsp.bits.Opcode === Comp & selSCompID === i.U
          when(hit) {
            w           := 0.U.asTypeOf(w)
            w.state     := DCUWState.Free
          }.otherwise {
            w.state     := DCUWState.SendComp
          }
        }
        is(DCUWState.SendComp) {
          val hit       = txRsp.fire & txRsp.bits.Opcode === Comp & selSCompID === i.U
          when(hit) {
            w           := 0.U.asTypeOf(w)
            w.state     := DCUWState.Free
          }
        }
      }
  }
  rxDat.ready := true.B


  /*
    * Write Req Buf Replace Read State
    */
  wBufRegVec.zipWithIndex.foreach {
    case (r, i) =>
      switch(r.replRState) {
        is(DCURState.Free) {
          val hit         = rxReq.fire & reqIsRepl & selRecWID === i.U
          when(hit) {
            r.replRState  := DCURState.ReadSram
            r.returnTxnID := rxReq.bits.ReturnTxnID
          }
        }
        is(DCURState.ReadSram) {
          val hit         = ds.map(_.io.earlyRReq.fire).reduce(_ | _) & earlyReplID === i.U & earlyRepl
          when(hit) {
            r.replRState  := DCURState.Reading
          }
        }
        is(DCURState.Reading) {
          r.returnTxnID   := 0.U
          r.replRState    := DCURState.Free
        }
      }
  }



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------- S1: Send Req To DataStorage ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
    * Send Write / Read Req
    */
  val sendWReqID = PriorityEncoder(wBufRegVec.map(_.state === DCUWState.Writting))
  val sendRReqID = PriorityEncoder(rBufRegVec.map(_.state === DCURState.Reading))
  ds.foreach(_.io.read  := DontCare)
  ds.foreach(_.io.write := DontCare)
  ds.zipWithIndex.foreach {
    case (d, i) =>
      val wHit = wBufRegVec(sendWReqID).state === DCUWState.Writting & wBufRegVec(sendWReqID).dsBank === i.U
      val rHit = rBufRegVec(sendRReqID).state === DCURState.Reading & rBufRegVec(sendRReqID).dsBank === i.U
      when(wHit) {
        d.io.write.index := wBufRegVec(sendWReqID).dsIndex
        d.io.write.data  := Cat(wBufRegVec(sendWReqID).data.map(_.bits).reverse)
        assert(wBufRegVec(sendWReqID).data.map(_.valid).reduce(_ & _))
      }.elsewhen(rHit) {
        d.io.read        := rBufRegVec(sendRReqID).dsIndex
      }
      assert(!(wHit & rHit))
  }
  assert(PopCount(rBufRegVec.map(_.state === DCURState.Reading))  <= 1.U)
  assert(PopCount(wBufRegVec.map(_.state === DCUWState.Writting)) <= 1.U)


  /*
    * Get Read CHI Resp
    */
  rRespQ.io.enq.valid := rRespReg.valid
  rRespQ.io.enq.bits  := rRespReg.bits


// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------- S2: Receive SRAM Resp and Send RxDat ------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
    * Receive SRAM Resp
    */
  val respId          = PriorityEncoder(respVec.map(_.valid))
  respVec             := ds.map(_.io.resp)
  rDatQ.io.enq.valid  := respVec.map(_.valid).reduce(_ | _)
  rDatQ.io.enq.bits.zipWithIndex.foreach { case (beat, i) =>
    beat := respVec(respId).bits(beatBits * (i + 1) - 1, beatBits * i)
  }
  assert(PopCount(respVec.map(_.valid)) <= 1.U)

  /*
    * Send Resp To CHI RxDat
    */
  sendBeatNumReg      := sendBeatNumReg + rxDat.fire.asUInt
  txDat.valid         := rRespQ.io.deq.valid & rDatQ.io.deq.valid
  txDat.bits          := rRespQ.io.deq.bits
  txDat.bits.Data     := rDatQ.io.deq.bits(sendBeatNumReg)
  txDat.bits.DataID   := toDataID(sendBeatNumReg)

  rRespQ.io.deq.ready := sendBeatNumReg === (nrBeat - 1).U & rxDat.fire
  rDatQ.io.deq.ready  := sendBeatNumReg === (nrBeat - 1).U & rxDat.fire



// ------------------------------------------------------------ Assertion ----------------------------------------------- //

  assert(Mux(rxReq.valid, rxReq.bits.Addr(fullAddrBits - 1, fullAddrBits - sTagBits) === 0.U, true.B))

  assert(Mux(rxReq.valid, rxReq.bits.Size === log2Ceil(djparam.blockBytes).U, true.B))

  assert(Mux(rDatQ.io.enq.valid, rDatQ.io.enq.ready, true.B))

  assert(Mux(rxReq.valid, rxReq.bits.Opcode === ReadNoSnp | rxReq.bits.Opcode === WriteNoSnpFull | rxReq.bits.Opcode === Replace, true.B))

}
