  package zhujiang.device.dma

  import chisel3._
  import chisel3.util._
  import org.chipsalliance.cde.config._
  import org.chipsalliance.cde.config.Parameters
  import zhujiang._
  import zhujiang.chi._
  import zhujiang.axi._
  import xs.utils.sram._
  import xijiang._
  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------- Module Define --------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
class ReadHandle(implicit p: Parameters) extends ZJModule {
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
  private val dataid         = WireInit(io.chi_rxdat.bits.DataID)

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
  rIdQueue.io.enq.bits.last  := axiEntrys(chiEntrys(selSendDatChiEReg).areid).sendDatNum === axiEntrys(chiEntrys(selSendDatChiEReg).areid).len + 1.U
  rIdQueue.io.deq.ready      := io.axi_r.ready

  /* 
   * axiEntry -> chiEntry Increment
   */

  private val sendHalfReq    = io.chi_txreq.ready && axiEntrys(selSendAxiEntry).state === AXIRState.Send && sramSelector.io.idleNum >= 1.U && axiEntrys(selSendAxiEntry).burst === BurstMode.Incr &&(axiEntrys(selSendAxiEntry).addr(5) && 
  axiEntrys(selSendAxiEntry).sendReqNum === 0.U || axiEntrys(selSendAxiEntry).len === axiEntrys(selSendAxiEntry).sendReqNum)
  private val sendFullReq    = io.chi_txreq.ready && axiEntrys(selSendAxiEntry).state === AXIRState.Send && sramSelector.io.idleNum >= 2.U &&
  axiEntrys(selSendAxiEntry).len + 1.U =/= axiEntrys(selSendAxiEntry).sendReqNum && axiEntrys(selSendAxiEntry).burst === BurstMode.Incr
  private val sendFixReq     = io.chi_txreq.ready && axiEntrys(selSendAxiEntry).state === AXIRState.Send && sramSelector.io.idleNum >= 1.U && 
  axiEntrys(selSendAxiEntry).len + 1.U =/= axiEntrys(selSendAxiEntry).sendReqNum && axiEntrys(selSendAxiEntry).burst === BurstMode.Fix
  private val sendWrapReq    = io.chi_txreq.ready && axiEntrys(selSendAxiEntry).state === AXIRState.Send && sramSelector.io.idleNum >= 1.U && 
  axiEntrys(selSendAxiEntry).len + 1.U =/= axiEntrys(selSendAxiEntry).sendReqNum && axiEntrys(selSendAxiEntry).burst === BurstMode.Wrap

  /* 
   * compute sendReqNum and Address
   */
  
  axiEntrys(selSendAxiEntry).sendReqNum := Mux(sendHalfReq | sendWrapReq | sendFixReq, axiEntrys(selSendAxiEntry).sendReqNum + 1.U, 
                                            Mux(sendFullReq, axiEntrys(selSendAxiEntry).sendReqNum + 2.U, axiEntrys(selSendAxiEntry).sendReqNum))
  axiEntrys(selSendAxiEntry).addr       := Mux(sendHalfReq, axiEntrys(selSendAxiEntry).addr + 32.U, 
                                            Mux(sendFullReq, axiEntrys(selSendAxiEntry).addr + 64.U, 
                                              Mux(sendWrapReq, (axiEntrys(selSendAxiEntry).addr + 32.U) & axiEntrys(selSendAxiEntry).byteMask | ~(~axiEntrys(selSendAxiEntry).addr | axiEntrys(selSendAxiEntry).byteMask), 
                                                axiEntrys(selSendAxiEntry).addr)))
  when(sendHalfReq | sendFixReq | sendWrapReq){
    chiEntrys(sramSelector.io.out0).areid := selSendAxiEntry
    chiEntrys(sramSelector.io.out0).full  := false.B
    chiEntrys(sramSelector.io.out0).last  := Mux(axiEntrys(selSendAxiEntry).addr(5), true.B, false.B)
    chiEntrys(sramSelector.io.out0).num   := axiEntrys(selSendAxiEntry).sendReqNum - axiEntrys(selSendAxiEntry).sendDatNum - (readSram.io.r.req.fire && chiEntrys(selSendDatChiEntry).areid === selSendAxiEntry).asUInt
    chiEntrys(sramSelector.io.out0).state := CHIRState.Wait
  }.elsewhen(sendFullReq){
    chiEntrys(sramSelector.io.out0).areid := selSendAxiEntry
    chiEntrys(sramSelector.io.out0).full  := true.B
    chiEntrys(sramSelector.io.out0).last  := false.B
    chiEntrys(sramSelector.io.out0).num   := axiEntrys(selSendAxiEntry).sendReqNum - axiEntrys(selSendAxiEntry).sendDatNum - (readSram.io.r.req.fire && chiEntrys(selSendDatChiEntry).areid === selSendAxiEntry).asUInt
    chiEntrys(sramSelector.io.out0).next  := sramSelector.io.out1
    chiEntrys(sramSelector.io.out0).state := CHIRState.Wait

    chiEntrys(sramSelector.io.out1).areid := selSendAxiEntry
    chiEntrys(sramSelector.io.out1).full  := true.B
    chiEntrys(sramSelector.io.out1).last  := true.B
    chiEntrys(sramSelector.io.out1).num   := axiEntrys(selSendAxiEntry).sendReqNum + 1.U - axiEntrys(selSendAxiEntry).sendDatNum - (readSram.io.r.req.fire && chiEntrys(selSendDatChiEntry).areid === selSendAxiEntry).asUInt
    chiEntrys(sramSelector.io.out1).state := CHIRState.Wait
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
  txReqFlit.Size   := Mux(axiEntrys(selSendAxiEntry).addr(raw - 1), axiEntrys(selSendAxiEntry).size, Mux(sendHalfReq | sendWrapReq | sendFixReq, "b101".U, "b110".U))
  txReqFlit.SrcID  := 1.U

  /* 
   * write data to sram
   */
  def fromCCNode(x: UInt): Bool = {
  require(x.getWidth == niw)
  val fromCC = WireInit(false.B)
  if(zjParams.localRing.filter(_.nodeType == NodeType.CC).nonEmpty){
    fromCC := zjParams.localRing.filter(_.nodeType == NodeType.CC).map(_.nodeId.asUInt >> nodeAidBits === x >> nodeAidBits).reduce(_ | _)
  }
  else {
    fromCC := false.B
  }
  fromCC
  }

  private val sramWrDatValidReg = RegNext(io.chi_rxdat.fire && (chiEntrys(dataTxnid).full ||
   !fromCCNode(io.chi_rxdat.bits.SrcID) || fromCCNode(io.chi_rxdat.bits.SrcID) && (chiEntrys(dataTxnid).last && dataid === 0.U || dataid === 2.U && !chiEntrys(dataTxnid).last)), false.B)
  private val sramWrDatReg      = RegEnable(io.chi_rxdat.bits.Data, io.chi_rxdat.fire)
  private val sramWrSet         = Mux(dataid === 2.U && chiEntrys(dataTxnid).full, chiEntrys(dataTxnid).next, dataTxnid)
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
  axiRFlit.last   := rIdQueue.io.deq.bits.last

  when(io.chi_rxdat.fire && dataid === 2.U && chiEntrys(dataTxnid).full){
    chiEntrys(chiEntrys(dataTxnid).next).state := CHIRState.SendData
    chiEntrys(dataTxnid).haveRecData2          := true.B
    chiEntrys(chiEntrys(dataTxnid).next).haveRecData1 := true.B
    chiEntrys(chiEntrys(dataTxnid).next).haveRecData2 := true.B
  }

    def byteMask(len:UInt) = {
    val maxShift = 1 << 3
    val tail = ((BigInt(1) << maxShift) - 1).U
    (Cat(len, tail) << 5.U) >> maxShift
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
          val hit    = io.axi_ar.fire && selFreeAxiEntry === i.U
          val nid    = PopCount(nidVec) - (io.axi_r.fire && io.axi_r.bits.last && io.axi_r.bits.id === io.axi_ar.bits.id).asUInt
          when(hit){
            a.state  := AXIRState.Send
            a.addr   := io.axi_ar.bits.addr & (~31.U(raw.W))
            a.arid   := io.axi_ar.bits.id
            a.len    := io.axi_ar.bits.len
            a.size   := io.axi_ar.bits.size
            a.burst  := io.axi_ar.bits.burst
            a.nid    := nid
            a.sendReqNum := 0.U
            a.sendDatNum := 0.U
            a.byteMask   := byteMask(io.axi_ar.bits.len)
           }
        }
        is(AXIRState.Send){
          val compHit = (a.sendReqNum === (a.len + 1.U)) && selSendAxiEntry === i.U
          val datNumAdd = readSram.io.r.req.fire && readSramAreid === i.U
          when(compHit){
            a.state := AXIRState.Comp
          }
          a.sendDatNum := Mux(datNumAdd, a.sendDatNum + 1.U, a.sendDatNum)
        }
        is(AXIRState.Comp){
          val hit = io.axi_r.fire && io.axi_r.bits.id === a.arid && a.nid === 0.U && io.axi_r.bits.last
          val datNumAdd = readSram.io.r.req.fire && readSramAreid === i.U
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
          val ReceDatHit = io.chi_rxdat.fire && dataTxnid === i.U && (io.chi_rxdat.bits.DataID === 0.U && (!fromCCNode(io.chi_rxdat.bits.SrcID) || c.full) ||
           fromCCNode(io.chi_rxdat.bits.SrcID) && !c.full && (c.last && dataid === 2.U || !c.last && dataid === 0.U))
          val receHit    = io.chi_rxrsp.fire && rspTxnid  === i.U && io.chi_rxrsp.bits.Opcode === RspOpcode.ReadReceipt
          when(ReceDatHit){
            c.haveRecData1   := true.B
            c.homeNid        := io.chi_rxdat.bits.HomeNID
            c.dbid           := io.chi_rxdat.bits.DBID
          }
          when(receHit){
            c.haveRecReceipt := true.B
          }
          when(c.haveRecData1 && c.haveRecReceipt){
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
          val hit = io.axi_r.fire & (c.haveRecData2 & c.full | !c.full)
          when(hit){
            c := 0.U.asTypeOf(c)
          }
        }
      }
  }
 
  //---------------------------------------------------------------------------------------------------------------------------------//
  //----------------------------------------------------------- IO Interface --------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  
  io.chi_txreq.valid   := sendFullReq | sendHalfReq | sendFixReq | sendWrapReq
  io.chi_txreq.bits    := txReqFlit
  io.axi_r.valid       := rDataQueue.io.deq.valid
  io.axi_r.bits        := axiRFlit
  io.chi_rxdat.ready   := true.B
  io.chi_rxrsp.ready   := true.B
  io.axi_ar.ready      := axiEntrys(selFreeAxiEntry).state === AXIRState.Free



/* 
 * assert logic
 */
  when(io.chi_rxdat.fire && dataid === 0.U){
    assert(!chiEntrys(dataTxnid).haveRecData1, "CHIEntrys haveRecDataFir logic is error")
  }
  when(io.chi_rxrsp.fire && io.chi_rxrsp.bits.Opcode === RspOpcode.ReadReceipt){
    assert(!chiEntrys(rspTxnid).haveRecReceipt, "CHIEntrys haveRecReceipt logic is error")
  }
  assert(dmaParams.axiEntrySize.U - PopCount(axiFreeVec) >= PopCount(chiSendDatVec), "Num of CHIEntrys is error")

  when(io.axi_ar.fire && io.axi_ar.bits.addr(raw - 1)){
    assert(io.axi_ar.bits.size <= 3.U & io.axi_ar.bits.len === 0.U, "AXIMaster Error!")
  }
  when(io.axi_ar.fire && io.axi_ar.bits.burst === BurstMode.Wrap){
    assert(PopCount(io.axi_ar.bits.len + 1.U) === 1.U, "AXIMaster send req is error")
  }
}