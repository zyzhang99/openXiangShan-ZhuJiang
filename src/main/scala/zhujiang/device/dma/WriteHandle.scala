  package zhujiang.device.dma


  import chisel3._
  import chisel3.util._
  import org.chipsalliance.cde.config._
  import freechips.rocketchip.amba.axi4._
  import org.chipsalliance.cde.config.Parameters
  import zhujiang._
  import zhujiang.chi._
  import zhujiang.axi._
  import xs.utils.sram._
  import zhujiang.device.dma.Encoder._


class WriteHandle(implicit p: Parameters) extends ZJModule{
  
  private val dmaParams = zjParams.dmaParams
  private val axiParams = AxiParams(dataBits = dw, addrBits = raw, idBits = dmaParams.idBits)

  val io = IO(new Bundle {
    // AXI4 Interface
    val axi_aw = Flipped(Decoupled(new AWFlit(axiParams)))
    val axi_w  = Flipped(Decoupled(new WFlit(axiParams)))
    val axi_b  = Decoupled(new BFlit(axiParams))

    // CHI Interface
    val chi_txreq = Decoupled(new ReqFlit)
    val chi_rxrsp = Flipped(Decoupled(new RespFlit))
    val chi_txrsp = Decoupled(new RespFlit)
    val chi_txdat = Decoupled(new DataFlit)
  })

  //---------------------------------------------------------------------------------------------------------------------------------//
  //-------------------------------------------------- Reg and Wire Define ----------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  private val axiEntrys     = RegInit(VecInit.fill(dmaParams.axiEntrySize)(0.U.asTypeOf(new AXIWEntry)))
  private val chiEntrys     = RegInit(VecInit.fill(dmaParams.chiEntrySize)(0.U.asTypeOf(new CHIWEntry)))
  private val dataSram      = Module(new SRAMTemplate(gen = UInt(dw.W), set = dmaParams.chiEntrySize, singlePort = true))
  private val maskSram      = Module(new SRAMTemplate(gen = UInt(bew.W), set = dmaParams.chiEntrySize, singlePort = true))
  private val rDataQueue    = Module(new Queue(gen = UInt(dw.W), entries = dmaParams.queueSize, flow = false, pipe = false))
  private val rMaskQueue    = Module(new Queue(gen = UInt(bew.W), entries = dmaParams.queueSize, flow = false, pipe = false))
  private val selEntryQueue = Module(new Queue(gen = UInt(log2Ceil(dmaParams.chiEntrySize).W), entries = dmaParams.queueSize, flow = false, pipe = false))

  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------------- Logic ----------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//

  private val rxRspTxnid     = Wire(UInt(log2Ceil(dmaParams.chiEntrySize).W))
  private val txReqTxnid     = Wire(UInt(log2Ceil(dmaParams.chiEntrySize).W))
  private val rxRspTxnidNext = Wire(UInt(log2Ceil(dmaParams.chiEntrySize).W))

  txReqTxnid                := io.chi_txreq.bits.TxnID(log2Ceil(dmaParams.chiEntrySize) - 1, 0)
  rxRspTxnid                := io.chi_rxrsp.bits.TxnID(log2Ceil(dmaParams.chiEntrySize) - 1, 0)
  rxRspTxnidNext            := rxRspTxnid + 1.U
  
  /* 
   * axiEntrys state vector and select
   */

  private val axiFreeVec    = axiEntrys.map(_.state === AXIWState.Free)
  private val axiBusyVec    = axiEntrys.map(_.state === AXIWState.ReceiveData)
  private val axiWrDataVec  = axiEntrys.map(a => a.state === AXIWState.ReceiveData && a.nid === 0.U)
  private val axiCompVec    = axiEntrys.map(_.state === AXIWState.Comp)

  private val selFreeAxiEntry   = PriorityEncoder(axiFreeVec)
  private val selWrDataAxiEntry = PriorityEncoder(axiWrDataVec)
  private val selCompAxiEntry   = PriorityEncoder(axiCompVec)

  /* 
   * chiEntrys state vector and select
   */

  private val chiBusyVec     = chiEntrys.map(c => c.state =/= CHIWState.Free && c.areid === selCompAxiEntry)
  private val chiSendDataVec = chiEntrys.map(_.state === CHIWState.SendWrData)
  private val chiSendReqVec  = chiEntrys.map(c => c.sendReqOrder === 0.U && c.state === CHIWState.SendWrReq)
  private val chiSendAckVec  = chiEntrys.map(c => c.state === CHIWState.SendCompAck && c.haveRecComp && c.sendAckOrder === 0.U)

  private val selSendReqChiEntry  = RREncoder(chiSendReqVec)
  private val selSendAckChiEntry  = RREncoder(chiSendAckVec)
  private val selSendDataChiEntry = PriorityEncoder(chiSendDataVec)

  /* 
   * chi flit define
   */
  private val txReqFlit         = Wire(new ReqFlit)
  private val txDatFlit         = Wire(new DataFlit)
  private val txRspFlit         = Wire(new RespFlit)


  /* 
   * merge reg or wire
   */

  private val wrBe             = io.axi_w.bits.strb.asTypeOf(VecInit.fill(dw/8)(0.U(1.W)))
  private val expendMask       = WireInit(VecInit.fill(dw/8)(0.U(8.W)))
  private val mergeDataReg     = RegInit(0.U(dw.W))
  private val mergeStrbReg     = RegInit(0.U(bew.W))
  private val shouldSendFull   = mergeStrbReg.andR

  /* 
   * Sram start index pointer and end index pointer
   */

  private val startIndex       = Reg(UInt(log2Ceil(dmaParams.chiEntrySize).W))
  private val endIndex         = Reg(UInt(log2Ceil(dmaParams.chiEntrySize).W))
  private val maybeFull        = RegInit(false.B)

  when(chiEntrys(startIndex + 1.U).state === CHIWState.Free && startIndex =/= endIndex && chiEntrys(startIndex).state === CHIWState.Free){
    startIndex := startIndex + 1.U
    maybeFull      := false.B
  }

  /* 
   * judge is ready to send axi b response
   */

  private val sendAxiBValid    = !chiBusyVec.reduce(_|_) && axiEntrys(selCompAxiEntry).state === AXIWState.Comp

  /* 
   * merge Data and Mask logic
   */

  when(io.axi_w.fire){
    expendMask.zip(wrBe).foreach{
      case(m,b) =>
        when(b === 1.U){
          m := 255.U
        }.otherwise{
          m := 0.U
        }
    }
  }
  private val mergeLast     = RegInit(false.B)
  private val mergeComp     = WireInit((mergeStrbReg(bew - 1) || mergeLast )&& RegNext(io.axi_w.fire))
  private val block         = WireInit(maybeFull && (endIndex === startIndex))
  private val judgeSendReq  = RegInit(true.B)
  private val sendReqNum    = chiEntrys.map(c => c.areid === selWrDataAxiEntry && (c.state === CHIWState.SendWrReq || c.state === CHIWState.WaitDBID) && !(c.full && c.last))
  private val waitCompEntryVec = chiEntrys.map(c => c.state =/= CHIWState.Free && !(c.full && c.last))

  when(io.axi_w.fire && io.axi_w.bits.last){
    mergeLast := true.B
  }.elsewhen(io.axi_w.fire){
    mergeLast := false.B
  }
  when(io.axi_w.fire && !mergeComp){
    mergeDataReg         := mergeDataReg | (expendMask.asTypeOf(UInt(dw.W)) & io.axi_w.bits.data)
    mergeStrbReg         := mergeStrbReg | io.axi_w.bits.strb
  }.elsewhen(io.axi_w.fire && mergeComp){
    mergeDataReg         := expendMask.asTypeOf(UInt(dw.W)) & io.axi_w.bits.data
    mergeStrbReg         := io.axi_w.bits.strb
  }.elsewhen(!io.axi_w.fire && mergeComp && !block){
    mergeDataReg         := 0.U.asTypeOf(mergeDataReg)
    mergeStrbReg         := 0.U.asTypeOf(mergeStrbReg)
  }
  
  when(!block && mergeComp){
    chiEntrys(endIndex).areid    := selWrDataAxiEntry
    chiEntrys(endIndex).sendFull := shouldSendFull
    chiEntrys(endIndex).state    := Mux(judgeSendReq, CHIWState.SendWrReq, CHIWState.WaitDBID)
    chiEntrys(endIndex).full     := Mux(mergeLast && judgeSendReq || axiEntrys(selWrDataAxiEntry).unalign || axiEntrys(selWrDataAxiEntry).burst =/= BurstMode.Incr, false.B, true.B)
    chiEntrys(endIndex).last     := Mux(axiEntrys(selWrDataAxiEntry).unalign || mergeLast || !judgeSendReq, true.B, false.B)
    chiEntrys(endIndex).mmioReq  := axiEntrys(selWrDataAxiEntry).addr(raw - 1)
    chiEntrys(endIndex).sendReqOrder := PopCount(sendReqNum) - (io.chi_rxrsp.fire && chiEntrys(rxRspTxnid).areid ===selWrDataAxiEntry && (io.chi_rxrsp.bits.Opcode === RspOpcode.DBIDResp ||
    io.chi_rxrsp.bits.Opcode === RspOpcode.CompDBIDResp)).asUInt 
    chiEntrys(endIndex).sendAckOrder := PopCount(waitCompEntryVec) - (io.chi_txrsp.fire).asUInt

    endIndex     := endIndex + 1.U
    maybeFull    := true.B
    judgeSendReq := Mux(mergeLast || axiEntrys(selWrDataAxiEntry).unalign || !judgeSendReq || axiEntrys(selWrDataAxiEntry).burst =/= BurstMode.Incr, true.B, false.B)
  }

  
  /* 
   * sram interface
   */
  dataSram.io.r.req.valid       := chiSendDataVec.reduce(_|_) && !dataSram.io.w.req.valid && rDataQueue.io.deq.ready
  dataSram.io.r.req.bits.setIdx := selSendDataChiEntry
  maskSram.io.r.req.valid       := chiSendDataVec.reduce(_|_) && !maskSram.io.w.req.valid
  maskSram.io.r.req.bits.setIdx := selSendDataChiEntry

  dataSram.io.w.req.valid        := Mux(!block && mergeComp, true.B, false.B)
  dataSram.io.w.req.bits.setIdx  := endIndex
  dataSram.io.w.req.bits.data(0) := mergeDataReg
  maskSram.io.w.req.valid        := Mux(!block && mergeComp, true.B, false.B)
  maskSram.io.w.req.bits.setIdx  := endIndex
  maskSram.io.w.req.bits.data(0) := mergeStrbReg

  /* 
   * state information
   */  
  when(io.chi_rxrsp.fire && chiEntrys(rxRspTxnid).full && (io.chi_rxrsp.bits.Opcode === RspOpcode.DBIDResp || io.chi_rxrsp.bits.Opcode === RspOpcode.CompDBIDResp)){
    chiEntrys(rxRspTxnidNext).state := CHIWState.SendWrData
    chiEntrys(rxRspTxnidNext).dbid  := io.chi_rxrsp.bits.DBID
    chiEntrys(rxRspTxnidNext).tgtID := io.chi_rxrsp.bits.SrcID
  }

  private val recDBIDAreid           = WireInit(chiEntrys(rxRspTxnid).areid)

  /* 
   * txFlit assignment
   */
  private val txReqOpcode    = WireInit(0.U.asTypeOf(UInt(7.W)))
  when(chiEntrys(selSendReqChiEntry).mmioReq && chiEntrys(selSendReqChiEntry).sendFull){
    txReqOpcode        := ReqOpcode.WriteNoSnpFull
  }.elsewhen(chiEntrys(selSendReqChiEntry).mmioReq && !chiEntrys(selSendReqChiEntry).sendFull){
    txReqOpcode        := ReqOpcode.WriteNoSnpPtl
  }.elsewhen(chiEntrys(selSendReqChiEntry).sendFull){
    txReqOpcode        := ReqOpcode.WriteUniqueFull
  }.elsewhen(!chiEntrys(selSendReqChiEntry).sendFull){
    txReqOpcode        := ReqOpcode.WriteUniquePtl
  }
  txReqFlit            := 0.U.asTypeOf(txReqFlit)
  txReqFlit.Addr       := axiEntrys(chiEntrys(selSendReqChiEntry).areid).addr
  txReqFlit.ExpCompAck := true.B
  txReqFlit.SrcID      := 1.U
  txReqFlit.Opcode     := txReqOpcode
  txReqFlit.TxnID      := selSendReqChiEntry
  txReqFlit.Size       := Mux(chiEntrys(selSendReqChiEntry).full, 6.U, 5.U)
  txReqFlit.Order      := "b10".U

  /* 
   * submodule interface
   */
  rDataQueue.io.enq.valid := RegNext(dataSram.io.r.req.fire, false.B)
  rDataQueue.io.enq.bits  := dataSram.io.r.resp.data(0)
  rDataQueue.io.deq.ready := io.chi_txdat.ready

  rMaskQueue.io.enq.valid := RegNext(maskSram.io.r.req.fire, false.B)
  rMaskQueue.io.enq.bits  := maskSram.io.r.resp.data(0)
  rMaskQueue.io.deq.ready := io.chi_txdat.ready

  selEntryQueue.io.enq.valid := RegNext(dataSram.io.r.req.fire, false.B)
  selEntryQueue.io.enq.bits  := RegNext(selSendDataChiEntry)
  selEntryQueue.io.deq.ready := io.chi_txdat.ready
  

  txDatFlit         := 0.U.asTypeOf(txDatFlit)
  txDatFlit.Opcode  := DatOpcode.NonCopyBackWriteData
  txDatFlit.SrcID   := 1.U
  txDatFlit.DataID  := Mux(chiEntrys(selEntryQueue.io.deq.bits).full && chiEntrys(selEntryQueue.io.deq.bits).last, 2.U, 0.U)
  txDatFlit.TxnID   := chiEntrys(selEntryQueue.io.deq.bits).dbid
  txDatFlit.TgtID   := chiEntrys(selEntryQueue.io.deq.bits).tgtID
  txDatFlit.Data    := rDataQueue.io.deq.bits
  txDatFlit.BE      := rMaskQueue.io.deq.bits

  txRspFlit           := 0.U.asTypeOf(txRspFlit)
  txRspFlit.Opcode    := RspOpcode.CompAck
  txRspFlit.SrcID     := 1.U
  txRspFlit.TgtID     := chiEntrys(selSendAckChiEntry).tgtID
  txRspFlit.TxnID     := chiEntrys(selSendAckChiEntry).dbid

  def byteMask(len:UInt) = {
    val maxShift = 1 << 3
    val tail = ((BigInt(1) << maxShift) - 1).U
    (Cat(len, tail) << 5.U) >> maxShift
  }
  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------------- FSM ------------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  axiEntrys.zipWithIndex.foreach{
    case(a, i) =>
      switch(a.state){
        is(AXIWState.Free){
          val hit = io.axi_aw.fire && selFreeAxiEntry === i.U
          when(hit){
            a.state   := AXIWState.ReceiveData
            a.addr    := io.axi_aw.bits.addr & (~31.U(48.W))
            a.unalign := io.axi_aw.bits.addr(5)
            a.burst   := io.axi_aw.bits.burst
            a.nid     := PopCount(axiBusyVec) - (mergeLast && RegNext(io.axi_w.fire)).asUInt
            a.awid    := io.axi_aw.bits.id
            a.byteMask := byteMask(io.axi_aw.bits.len)
          }
        }
        is(AXIWState.ReceiveData){
          val hit       = a.nid === 0.U && mergeLast && RegNext(io.axi_w.fire)
          val reduceNid = a.nid =/= 0.U && mergeLast && RegNext(io.axi_w.fire)
          val addrIncr  = chiEntrys(txReqTxnid).areid === i.U && io.chi_txreq.fire && a.burst === BurstMode.Incr
          val addrWrap  = chiEntrys(txReqTxnid).areid === i.U && io.chi_txreq.fire && a.burst === BurstMode.Wrap
          val alignHit  = a.nid === 0.U && RegNext(io.axi_w.fire)
          a.addr       := Mux(addrIncr, Mux(txReqFlit.Size === 5.U, a.addr + 32.U, a.addr + 64.U), Mux(addrWrap, a.addr + 32.U & a.byteMask | ~(~a.addr | a.byteMask), a.addr))
          when(alignHit){
            a.unalign := false.B
          }
          when(hit){
            a.state := AXIWState.Comp
          }
          when(reduceNid){
            a.nid := a.nid - 1.U
          }
        }
        is(AXIWState.Comp){
          val hit       = a.nid === 0.U && io.axi_b.bits.id === a.awid && io.axi_b.fire 
          val addrIncr  = chiEntrys(txReqTxnid).areid === i.U && io.chi_txreq.fire && a.burst === BurstMode.Incr
          val addrWrap  = chiEntrys(txReqTxnid).areid === i.U && io.chi_txreq.fire && a.burst === BurstMode.Wrap
          a.addr       := Mux(addrIncr, Mux(txReqFlit.Size === 5.U, a.addr + 32.U, a.addr + 64.U), Mux(addrWrap, a.addr + 32.U & a.byteMask | ~(~a.addr | a.byteMask), a.addr))
          when(hit){
            a := 0.U.asTypeOf(a)
          }
        }
      }
  }
  chiEntrys.foreach{
    case(c) =>
      when(c.state =/= CHIWState.Free && c.sendAckOrder =/= 0.U && io.chi_txrsp.fire){
        c.sendAckOrder := c.sendAckOrder - 1.U
      }
  }
  chiEntrys.zipWithIndex.foreach{
    case(c, i) =>
      when(c.state =/= CHIWState.Free && io.chi_rxrsp.fire && io.chi_rxrsp.bits.TxnID === i.U && (io.chi_rxrsp.bits.Opcode === RspOpcode.Comp || io.chi_rxrsp.bits.Opcode === RspOpcode.CompDBIDResp)){
        c.haveRecComp := true.B
      }
  }

  chiEntrys.zipWithIndex.foreach{
    case(c, i) =>
      switch(c.state){
        is(CHIWState.SendWrReq){
          val hit = selSendReqChiEntry === i.U && io.chi_txreq.fire
          val reduceHit = io.chi_rxrsp.fire && recDBIDAreid === c.areid &&c.sendReqOrder =/= 0.U && (io.chi_rxrsp.bits.Opcode === RspOpcode.DBIDResp || io.chi_rxrsp.bits.Opcode === RspOpcode.CompDBIDResp)
          when(hit){
            c.state := CHIWState.WaitDBID
          }
          when(reduceHit){
            c.sendReqOrder := c.sendReqOrder - 1.U
          }
        }
        is(CHIWState.WaitDBID){
          val hit = io.chi_rxrsp.fire && io.chi_rxrsp.bits.TxnID === i.U && (io.chi_rxrsp.bits.Opcode === RspOpcode.CompDBIDResp || io.chi_rxrsp.bits.Opcode === RspOpcode.DBIDResp)
          when(hit){
            c.state    := CHIWState.SendWrData
            c.dbid     := io.chi_rxrsp.bits.DBID
            c.tgtID    := io.chi_rxrsp.bits.SrcID
          }
        }
        is(CHIWState.SendWrData){
          val hit = dataSram.io.r.req.fire && dataSram.io.r.req.bits.setIdx === i.U
          when(hit){
            c.state := CHIWState.WaitDataSend
          }
        }
        is(CHIWState.WaitDataSend){
          val hit = io.chi_txdat.fire && io.chi_txdat.bits.TxnID === c.dbid &&
          (io.chi_txdat.bits.DataID === 0.U && !(c.full && c.last) || io.chi_txdat.bits.DataID === 2.U && c.last && c.full)
          when(hit & c.last & c.full){
            c := 0.U.asTypeOf(c)
          }.elsewhen(hit){
            c.state := CHIWState.SendCompAck
          }
        }
        is(CHIWState.SendCompAck){
          val hit = io.chi_txrsp.fire && io.chi_txrsp.bits.TxnID === c.dbid && c.sendAckOrder === 0.U && c.haveRecComp 
          when(hit){
            c := 0.U.asTypeOf(c)
          }
        }
      }
  }
  //---------------------------------------------------------------------------------------------------------------------------------//
  //--------------------------------------------------------- IO Logic --------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//

  /* 
   * CHI interface
   */
  io.chi_txreq.bits   := txReqFlit
  io.chi_txreq.valid  := chiSendReqVec.reduce(_|_)
  io.chi_txdat.valid  := selEntryQueue.io.deq.valid
  io.chi_txdat.bits   := txDatFlit
  io.chi_rxrsp.ready  := true.B
  io.chi_txrsp.valid  := chiSendAckVec.reduce(_|_)
  io.chi_txrsp.bits   := txRspFlit
  

  /* 
   * AXI interface
   */
  io.axi_aw.ready     := axiFreeVec.reduce(_|_)
  io.axi_w.ready      := !(block && mergeComp) && axiBusyVec.reduce(_|_)
  io.axi_b.valid      := sendAxiBValid 
  io.axi_b.bits       := 0.U.asTypeOf(io.axi_b.bits)
  io.axi_b.bits.id    := axiEntrys(selCompAxiEntry).awid

  //---------------------------------------------------------------------------------------------------------------------------------//
  //--------------------------------------------------------- Assertion -------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  assert(dmaParams.axiEntrySize.U - PopCount(axiFreeVec) >= PopCount(chiSendAckVec), "sendAckOrder is error")
  assert(dmaParams.axiEntrySize.U - PopCount(axiFreeVec) >= PopCount(chiSendReqVec), "sendReqOrder is error")
  when(io.chi_rxrsp.fire && (io.chi_rxrsp.bits.Opcode === RspOpcode.Comp || io.chi_rxrsp.bits.Opcode === RspOpcode.CompDBIDResp)){
    assert(!chiEntrys(rxRspTxnid).haveRecComp, "haveRecComp logic is error, chiEntry: %d", rxRspTxnid)
  when(io.axi_aw.fire && io.axi_aw.bits.addr(raw - 1)){
    assert(io.axi_aw.bits.size <= 3.U & io.axi_aw.bits.len === 0.U, "AXIMaster Error!")
  }
  }

}