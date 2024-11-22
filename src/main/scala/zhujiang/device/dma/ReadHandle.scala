  package zhujiang.device.dma

  import chisel3._
  import chisel3.util._
  import org.chipsalliance.cde.config._
  import org.chipsalliance.cde.config.Parameters
  import zhujiang._
  import zhujiang.chi._
  import zhujiang.axi._
  import xs.utils.sram._

  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------- Module Define --------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
class ReadHandle(implicit p: Parameters) extends ZJModule{
  private val dmaParams = zjParams.dmaParams
  private val axiParams = AxiParams(dataBits = dw, addrBits = raw, idBits = dmaParams.idBits)
  val io = IO(new Bundle {
    // AXI4 Interface
    val axi_ar = Flipped(Decoupled(new ARFlit(axiParams)))
    val axi_r  = Decoupled(new RFlit(axiParams))

    // CHI Interface
    val chi_txreq = Decoupled(new ReqFlit)
    val chi_rxrsp = Flipped(Decoupled(new RespFlit))
    val chi_rxdat = Flipped(Decoupled(new DataFlit))
  })
  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------- Reg and Wire Define --------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  private val axiEntrys      = RegInit(VecInit.fill(dmaParams.axiEntrySize)(0.U.asTypeOf(new AXIREntry)))
  private val chiEntrys      = RegInit(VecInit.fill(dmaParams.chiEntrySize)(0.U.asTypeOf(new CHIREntry)))

  private val sramSelector   = Module(new SRAMSelector)
  private val readSram       = Module(new SRAMTemplate(gen = UInt(dw.W), set = dmaParams.chiEntrySize, singlePort = true))
  private val rIdQueue       = Module(new Queue(gen = new IDBundle, entries = dmaParams.queueSize, flow = false, pipe = false))
  private val rDataQueue     = Module(new Queue(gen = UInt(dw.W), entries = dmaParams.queueSize, flow = false, pipe = false))

  private val dataTxnid      = WireInit(io.chi_rxdat.bits.TxnID(log2Ceil(dmaParams.chiEntrySize) - 1, 0))
  private val rspTxnid       = WireInit(io.chi_rxrsp.bits.TxnID(log2Ceil(dmaParams.chiEntrySize) - 1, 0))  

  //---------------------------------------------------------------------------------------------------------------------------------//
  //----------------------------------------------------------- Logic ---------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  private val axiFreeVec      = axiEntrys.map(_.state === AXIRState.Free)
  private val selFreeAxiEntry = PriorityEncoder(axiFreeVec)
  private val axiSendVec      = axiEntrys.map(_.state === AXIRState.Send)
  private val selSendAxiEntry = PriorityEncoder(axiSendVec)

  private val nidVec          = axiEntrys.map(a => a.arid === io.axi_ar.bits.id && a.state =/= AXIRState.Free)


  private val chiFreeVec     = chiEntrys.map(_.state === CHIRState.Free)
  private val chiSendDatVec  = chiEntrys.map(c => c.state === CHIRState.SendData && c.num === 0.U && axiEntrys(c.areid).nid === 0.U)

  private val selSendDatChiEntry = PriorityEncoder(chiSendDatVec)
  private val selSendDatChiEReg  = RegNext(selSendDatChiEntry)

  /* 
   * submodule interface
   */

  sramSelector.io.idle    := chiFreeVec

  rDataQueue.io.enq.valid := RegNext(readSram.io.r.req.fire, false.B)
  rDataQueue.io.enq.bits  := readSram.io.r.resp.data(0)
  rDataQueue.io.deq.ready := io.axi_r.ready

  rIdQueue.io.enq.valid   := RegNext(readSram.io.r.req.fire, false.B)
  rIdQueue.io.enq.bits.areid := chiEntrys(selSendDatChiEReg).areid
  rIdQueue.io.enq.bits.rid   := axiEntrys(chiEntrys(selSendDatChiEReg).areid).arid
  rIdQueue.io.deq.ready      := io.axi_r.ready

  /* 
   * axiEntry -> chiEntry
   */

  private val sendHalfReq    = io.chi_txreq.ready && axiEntrys(selSendAxiEntry).state === AXIRState.Send && sramSelector.io.idleNum >= 1.U && (axiEntrys(selSendAxiEntry).addr(5) &&
  axiEntrys(selSendAxiEntry).sendReqNum === 0.U || axiEntrys(selSendAxiEntry).len === axiEntrys(selSendAxiEntry).sendReqNum)
  private val sendFullReq    = io.chi_txreq.ready && axiEntrys(selSendAxiEntry).state === AXIRState.Send && sramSelector.io.idleNum >= 2.U && axiEntrys(selSendAxiEntry).len + 1.U =/= axiEntrys(selSendAxiEntry).sendReqNum

  axiEntrys(selSendAxiEntry).sendReqNum := Mux(sendHalfReq, axiEntrys(selSendAxiEntry).sendReqNum + 1.U, 
                                            Mux(sendFullReq, axiEntrys(selSendAxiEntry).sendReqNum + 2.U, axiEntrys(selSendAxiEntry).sendReqNum))
  when(sendHalfReq){
    chiEntrys(sramSelector.io.out0).areid := selSendAxiEntry
    chiEntrys(sramSelector.io.out0).full  := false.B
    chiEntrys(sramSelector.io.out0).last  := true.B
    chiEntrys(sramSelector.io.out0).num   := axiEntrys(selSendAxiEntry).sendReqNum - axiEntrys(selSendAxiEntry).sendDatNum - (io.axi_r.fire && 
    io.axi_r.bits.id === axiEntrys(selSendAxiEntry).arid && axiEntrys(selSendAxiEntry).nid === 0.U).asUInt
    chiEntrys(sramSelector.io.out0).state := CHIRState.Wait
    
    axiEntrys(selSendAxiEntry).addr       := axiEntrys(selSendAxiEntry).addr + 32.U
  }.elsewhen(sendFullReq){
    chiEntrys(sramSelector.io.out0).areid := selSendAxiEntry
    chiEntrys(sramSelector.io.out0).full  := true.B
    chiEntrys(sramSelector.io.out0).last  := false.B
    chiEntrys(sramSelector.io.out0).num   := axiEntrys(selSendAxiEntry).sendReqNum - axiEntrys(selSendAxiEntry).sendDatNum - (io.axi_r.fire && 
    io.axi_r.bits.id === axiEntrys(selSendAxiEntry).arid && axiEntrys(selSendAxiEntry).nid === 0.U).asUInt
    chiEntrys(sramSelector.io.out0).next  := sramSelector.io.out1
    chiEntrys(sramSelector.io.out0).state := CHIRState.Wait

    chiEntrys(sramSelector.io.out1).areid := selSendAxiEntry
    chiEntrys(sramSelector.io.out1).full  := true.B
    chiEntrys(sramSelector.io.out1).last  := true.B
    chiEntrys(sramSelector.io.out1).num   := axiEntrys(selSendAxiEntry).sendReqNum + 1.U - axiEntrys(selSendAxiEntry).sendDatNum - (io.axi_r.fire &&
    io.axi_r.bits.id === axiEntrys(selSendAxiEntry).arid && axiEntrys(selSendAxiEntry).nid === 0.U).asUInt
    chiEntrys(sramSelector.io.out1).state := CHIRState.Wait

    axiEntrys(selSendAxiEntry).addr       := axiEntrys(selSendAxiEntry).addr + 64.U
  }
  /* 
   * txReqFlit assignment
   */
  
  private val txReqFlit     = Wire(new ReqFlit)
  txReqFlit        := 0.U.asTypeOf(txReqFlit)
  txReqFlit.Addr   := axiEntrys(selSendAxiEntry).addr
  txReqFlit.Opcode := Mux(axiEntrys(selSendAxiEntry).addr(raw - 1), ReqOpcode.ReadNoSnp, ReqOpcode.ReadOnce)
  txReqFlit.Order  := "b11".U
  txReqFlit.TxnID  := sramSelector.io.out0
  txReqFlit.Size   := Mux(sendHalfReq, "b101".U, "b110".U)
  txReqFlit.SrcID  := 1.U

  /* 
   * write data to sram
   */

  private val sramWrDatValidReg = RegNext(io.chi_rxdat.fire, false.B)
  private val sramWrDatReg      = RegEnable(io.chi_rxdat.bits.Data, io.chi_rxdat.fire)
  private val sramWrSet         = Mux(io.chi_rxdat.bits.DataID === 2.U, chiEntrys(dataTxnid).next, dataTxnid)
  private val sramWrSetReg      = RegNext(sramWrSet)
  private val readSramAreid     = WireInit(chiEntrys(selSendDatChiEntry).areid)


  readSram.io.w.req.valid        := sramWrDatValidReg
  readSram.io.w.req.bits.setIdx  := sramWrSetReg
  readSram.io.w.req.bits.data(0) := sramWrDatReg

  /* 
   * read data from sram
   */

  readSram.io.r.req.valid       := chiEntrys(selSendDatChiEntry).state === CHIRState.SendData && !readSram.io.w.req.valid && rDataQueue.io.deq.ready
  readSram.io.r.req.bits.setIdx := selSendDatChiEntry
  
  private val axiRFlit     = Wire(new RFlit(axiParams))
  axiRFlit        := 0.U.asTypeOf(axiRFlit)
  axiRFlit.data   := rDataQueue.io.deq.bits
  axiRFlit.id     := rIdQueue.io.deq.bits.rid
  axiRFlit.last   := axiEntrys(rIdQueue.io.deq.bits.areid).sendDatNum === axiEntrys(rIdQueue.io.deq.bits.areid).len

  when(io.chi_rxdat.fire && io.chi_rxdat.bits.DataID === 2.U){
    chiEntrys(chiEntrys(dataTxnid).next).state := CHIRState.SendData
  }

  /* several scenarios for transmission
   *
   * the master must not wait for the slave to assert ARREADY before asserting ARVALID
   * 
   * axi_ar  valid _____/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\________________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\______
   *         ready ___________/⎺⎺⎺⎺⎺\______________________/⎺⎺⎺⎺⎺⎺\______ 
   *                               ↖__ the slave can wait for ARVALID to 
   *                                   asserted before it asserts ARREADY 
   * 
   * axi_ar  valid _____/⎺⎺⎺⎺⎺\______________________/⎺⎺⎺⎺⎺\_____________
   *         ready ⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
   *                      ↖__ the slave laso can assert ARREADY before
   *                           ARREADY is asserted
   * 
   * the slave must wait for both ARVALID and ARREADY to be asserted before it asserts
   * RVALID to indicate that valid data is available
   * 
   * axi_r   valid _____/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\________________/⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\______
   *         ready ___________/⎺⎺⎺⎺⎺\_______________________/⎺⎺⎺⎺⎺\______
   *                              ↖__!!! the slave must not wait for the master to
   *                                     assert RREADY before asserting RVALID
   * 
   * axi_r  valid _____/⎺⎺⎺⎺⎺\______________________/⎺⎺⎺⎺⎺\_____________
   *        ready ⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
   *                       ↖__master can assert RREADY before RVALID is asserted
   */

  //---------------------------------------------------------------------------------------------------------------------------------//
  //-------------------------------------------------------- State Transfer ---------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  axiEntrys.zipWithIndex.foreach{
    case(a, i) =>
      switch(a.state){
        is(CHIRState.Free){
          val hit = io.axi_ar.fire && selFreeAxiEntry === i.U
          val nid = PopCount(nidVec) - (io.axi_r.fire && io.axi_r.bits.last && io.axi_r.bits.id === io.axi_ar.bits.id).asUInt
          when(hit){
            a.state  := AXIRState.Send
            a.addr   := io.axi_ar.bits.addr & (~31.U(raw.W))
            a.arid   := io.axi_ar.bits.id
            a.len    := io.axi_ar.bits.len
            a.burst  := io.axi_ar.bits.burst
            a.nid    := nid
            a.sendReqNum := 0.U
            a.sendDatNum := 0.U
          }
        }
        is(AXIRState.Send){
          val compHit = (a.sendReqNum === (a.len + 1.U)) && selSendAxiEntry === i.U
          val datNumAdd = io.axi_r.fire && io.axi_r.bits.id === a.arid && a.nid === 0.U
          when(compHit){
            a.state := AXIRState.Comp
          }
          a.sendDatNum := Mux(datNumAdd, a.sendDatNum + 1.U, a.sendDatNum)
        }
        is(AXIRState.Comp){
          val hit = io.axi_r.fire && io.axi_r.bits.id === a.arid && a.nid === 0.U && io.axi_r.bits.last
          val datNumAdd = io.axi_r.fire && io.axi_r.bits.id === a.arid && a.nid === 0.U
          a.sendDatNum := Mux(datNumAdd, a.sendDatNum + 1.U, a.sendDatNum)
          when(hit){
            a := 0.U.asTypeOf(a)
          }
        }
      }
  }
  axiEntrys.foreach{
    case(a) =>
      when(a.state =/= AXIRState.Free && a.nid =/= 0.U && io.axi_r.fire && io.axi_r.bits.last && io.axi_r.bits.id === a.arid){
        a.nid := a.nid - 1.U
      }
  }

  chiEntrys.foreach{
    case(c) =>
      when(c.num =/= 0.U && c.areid === readSramAreid && readSram.io.r.req.fire){
        c.num := c.num - 1.U
      }
  }
  
  chiEntrys.zipWithIndex.foreach{
    case(c, i) =>
      switch(c.state){
        is(CHIRState.Wait){
          val ReceDatHit = io.chi_rxdat.fire && io.chi_rxdat.bits.DataID === 0.U && dataTxnid === i.U
          val receHit    = io.chi_rxrsp.fire && rspTxnid  === i.U && io.chi_rxrsp.bits.Opcode === RspOpcode.ReadReceipt
          when(ReceDatHit){
            c.haveRecData := true.B
            c.homeNid        := io.chi_rxdat.bits.HomeNID
            c.dbid           := io.chi_rxdat.bits.DBID
          }
          when(receHit){
            c.haveRecReceipt := true.B
          }
          when(c.haveRecData && c.haveRecReceipt){
            c.state := CHIRState.SendData
          }
        }
        is(CHIRState.SendData){
          val hit = readSram.io.r.req.fire && readSram.io.r.req.bits.setIdx === i.U
          when(hit){
            c.state := CHIRState.Comp
          }
        }
        is(CHIRState.Comp){
          val hit = io.axi_r.fire 
          when(hit){
            c := 0.U.asTypeOf(c)
          }
        }
      }
  }
 
  //---------------------------------------------------------------------------------------------------------------------------------//
  //----------------------------------------------------------- IO Interface --------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  
  io.chi_txreq.valid   := sendFullReq || sendHalfReq
  io.chi_txreq.bits    := txReqFlit
  io.axi_r.valid       := rDataQueue.io.deq.valid
  io.axi_r.bits        := axiRFlit
  io.chi_rxdat.ready   := true.B
  io.chi_rxrsp.ready   := true.B
  io.axi_ar.ready      := axiEntrys(selFreeAxiEntry).state === AXIRState.Free



/* 
 * assert logic
 */
  when(io.chi_rxdat.fire && io.chi_rxdat.bits.DataID === 0.U){
    assert(!chiEntrys(dataTxnid).haveRecData, "CHIEntrys haveRecDataFir logic is error")
  }
  when(io.chi_rxrsp.fire && io.chi_rxrsp.bits.Opcode === RspOpcode.ReadReceipt){
    assert(!chiEntrys(rspTxnid).haveRecReceipt, "CHIEntrys haveRecReceipt logic is error")
  }
  assert(dmaParams.axiEntrySize.U - PopCount(axiFreeVec) >= PopCount(chiSendDatVec), "Num of CHIEntrys is error")
}