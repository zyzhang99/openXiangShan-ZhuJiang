package dongjiang.pcu

import dongjiang._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import dongjiang.utils.Encoder.RREncoder
import xs.utils.perf.HasPerfLogging

/*
 * ID Transfer:
 *
 *
 * { dbRCReq  } Read / Clean Req To DataBuffer      { to }                { dbID }
 * { getDBID  } Get DBID Req To DataBuffer          { from }  { entryID }
 * { dbidResp } Resp With DBID From DataBuffer      { to }    { entryID } { dbID }
 * { dataTDB  } Send Data To DataBufer                                    { dbID }
 * { dataFDB  } Send Data From DataBuffer           { to }                { dbID }
 *
 */

object DBState {
  val width       = 3
  // FREE -> ALLOC -> FREE
  // FREE -> ALLOC -> READ(needClean)  -> READING(needClean)  -> FREE
  // FREE -> ALLOC -> READ(!needClean) -> READING(!needClean) -> READ_DONE -> READ(needClean) -> READING(needClean) -> FREE
  // FREE -> ALLOC -> READ(!needClean) -> READING(!needClean) -> READ2APU -> WAIT_APU -> READ_DONE
  val FREE        = "b000".U
  val ALLOC       = "b001".U
  val READ        = "b010".U // Ready to read
  val READING     = "b011".U // Already partially read
  val READ_DONE   = "b100".U // Has been read all beat
  val READ2APU    = "b101".U
  val WAIT_APU    = "b110".U
}


class DBEntry(implicit p: Parameters) extends DJBundle with HasToIncoID {
  val state       = UInt(DBState.width.W)
  val rBeatNum    = UInt(log2Ceil(nrBeat).W)
  val rBeatOH     = UInt(2.W)
  val needClean   = Bool()
  val beats       = Vec(nrBeat, Valid(new Bundle {
    val data      = UInt(beatBits.W)
    val mask      = UInt(maskBits.W)
  })) // TODO: Reg -> SRAM

  def getBeat     = beats(Mux(rBeatOH === "b10".U, 1.U, rBeatNum)).bits
  def isLast      = Mux(rBeatOH === "b11".U, rBeatNum === 1.U, rBeatNum === 0.U)
  def isFree      = state === DBState.FREE
  def isAlloc     = state === DBState.ALLOC
  def isRead      = state === DBState.READ
  def isReading   = state === DBState.READING
  def isReadDone  = state === DBState.READ_DONE
  def isRead2APU  = state === DBState.READ2APU
  def isWaitAPU   = state === DBState.WAIT_APU
  def canRecReq   = isAlloc | isReadDone
}


class DataBuffer()(implicit p: Parameters) extends DJModule with HasPerfLogging {
// --------------------- IO declaration ------------------------//
  val io = IO(Flipped(new DBBundle(hasDBRCReq = true)))

  // Del it
  dontTouch(io)

  val apu           = Module(new AtomicProcessUnit())

// --------------------- Reg and Wire declaration ------------------------//
  val entrys        = RegInit(VecInit(Seq.fill(djparam.nrDatBuf) { 0.U.asTypeOf(new DBEntry()) }))
  val apuEntryInit  = WireInit(0.U.asTypeOf(new APUEntry())); apuEntryInit.op := AtomicOp.NONE
  val apuEntrys     = RegInit(VecInit(Seq.fill(djparam.nrAPU)    { apuEntryInit }))
  // dbidResp
  val dbidRespQ     = Module(new Queue(new DBIDResp(), 1, flow = false, pipe = true))


// ---------------------------------------------------------------------------------------------------------------------- //
// -------------------------------------------------- GetDBID & DBIDResp ------------------------------------------------ //
// ---------------------------------------------------------------------------------------------------------------------- //
  val dbFreeVec                 = entrys.map(_.isFree)
  val dbWithApuFreeVec          = dbFreeVec.reverse.slice(0, djparam.nrAPU)
  val reqIsAtomic               = io.getDBID.bits.atomicVal
  val getDBIDId                 = Mux(reqIsAtomic, Fill(dbIdBits, 1.U(1.W)) - PriorityEncoder(dbWithApuFreeVec), PriorityEncoder(dbFreeVec))
  // receive
  io.getDBID.ready              := dbidRespQ.io.enq.ready
  dbidRespQ.io.enq.valid        := io.getDBID.valid
  dbidRespQ.io.enq.bits.retry   := Mux(reqIsAtomic, !dbWithApuFreeVec.reduce(_ | _),  !dbFreeVec.reduce(_ | _))
  dbidRespQ.io.enq.bits.dbID    := getDBIDId
  dbidRespQ.io.enq.bits.to      := io.getDBID.bits.from
  dbidRespQ.io.enq.bits.entryID := io.getDBID.bits.entryID
  // output
  io.dbidResp                   <> dbidRespQ.io.deq


// ---------------------------------------------------------------------------------------------------------------------- //
// ----------------------------------------------------- DATA TO DB ----------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  when(io.dataTDB.valid & !io.dataTDB.bits.atomicVal) {
    entrys(io.dataTDB.bits.dbID).beats(toBeatNum(io.dataTDB.bits.dataID)).valid     := true.B
    entrys(io.dataTDB.bits.dbID).beats(toBeatNum(io.dataTDB.bits.dataID)).bits.data := io.dataTDB.bits.data
    entrys(io.dataTDB.bits.dbID).beats(toBeatNum(io.dataTDB.bits.dataID)).bits.mask := io.dataTDB.bits.mask
    assert(!entrys(io.dataTDB.bits.dbID).beats(toBeatNum(io.dataTDB.bits.dataID)).valid)
  }.elsewhen(io.dataTDB.valid & io.dataTDB.bits.atomicVal) {
    assert(io.dataTDB.bits.dbID >= nrDBWithoutAPUs.U)
    apuEntrys(io.dataTDB.bits.apuEID).atomic.data := io.dataTDB.bits.data
    apuEntrys(io.dataTDB.bits.apuEID).atomic.mask := io.dataTDB.bits.mask
    apuEntrys(io.dataTDB.bits.apuEID).initOff     := io.dataTDB.bits.dataID(1)
  }
  io.dataTDB.ready := true.B


// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------------- RC REQ TO DB ---------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  when(io.dbRCReqOpt.get.fire) {
    entrys(io.dbRCReqOpt.get.bits.dbID).needClean := io.dbRCReqOpt.get.bits.isClean
    entrys(io.dbRCReqOpt.get.bits.dbID).to        := io.dbRCReqOpt.get.bits.to
    entrys(io.dbRCReqOpt.get.bits.dbID).rBeatOH   := io.dbRCReqOpt.get.bits.rBeatOH
    assert(Mux(io.dbRCReqOpt.get.bits.isRead, io.dbRCReqOpt.get.bits.rBeatOH =/= 0.U, true.B))
  }
  io.dbRCReqOpt.get.ready := entrys(io.dbRCReqOpt.get.bits.dbID).canRecReq
  when(io.dbRCReqOpt.get.valid) {
    assert(!entrys(io.dbRCReqOpt.get.bits.dbID).isFree)
    assert(entrys(io.dbRCReqOpt.get.bits.dbID).beats.map(_.valid).reduce(_ | _))
    assert(!(io.dbRCReqOpt.get.bits.exuAtomic & io.dbRCReqOpt.get.bits.isClean))
  }

// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------------- DATA TO NODE ---------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  // TODO: Ensure that the order of dataFDB Out is equal to the order of dbRCReq In
  val readVec     = entrys.map(_.isRead)
  val readingVec  = entrys.map(_.isReading)
  val hasReading  = readingVec.reduce(_ | _)
  val readId      = RREncoder(readVec)
  val readingId   = PriorityEncoder(readingVec)
  val selReadId   = Mux(hasReading, readingId, readId)
  assert(PopCount(readingVec) <= 1.U)

  io.dataFDB.valid            := readVec.reduce(_ | _) | readingVec.reduce(_ | _)
  io.dataFDB.bits.data        := entrys(selReadId).getBeat.data
  io.dataFDB.bits.dataID      := toDataID(entrys(selReadId).rBeatNum)
  io.dataFDB.bits.dbID        := selReadId
  io.dataFDB.bits.mask        := entrys(selReadId).getBeat.mask
  io.dataFDB.bits.to          := entrys(selReadId).to
  entrys(selReadId).rBeatNum  := entrys(selReadId).rBeatNum + io.dataFDB.fire.asUInt // TODO: optimize reg power

  when(io.dataFDB.valid) {
    assert(entrys(io.dataFDB.bits.dbID).beats(toBeatNum(io.dataTDB.bits.dataID)).valid)
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------------- READ DATA TO APU ------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  val read2ApuVec     = entrys.map(_.isRead2APU)
  val read2ApuDBID    = PriorityEncoder(read2ApuVec)
  val read2ApuAPID    = ((djparam.nrDatBuf - 1).U - read2ApuDBID)(apuIdBits - 1, 0)
  assert(PopCount(read2ApuVec) <= 1.U)

  apu.io.in.valid         := read2ApuVec.reduce(_ | _)
  apu.io.in.bits.dbID     := read2ApuDBID
  apu.io.in.bits.op       := apuEntrys(read2ApuAPID).op
  apu.io.in.bits.atomic   := apuEntrys(read2ApuAPID).atomic
  apu.io.in.bits.data     := entrys(read2ApuDBID).beats(apuEntrys(read2ApuAPID).initOff.asUInt).bits.data
  assert(entrys(read2ApuDBID).beats(apuEntrys(read2ApuAPID).initOff.asUInt).valid | !apu.io.in.valid)

// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------ RECEIVE DATA FROM APU ----------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  when(apu.io.out.valid) {
    entrys(apu.io.out.bits.dbID).beats(apuEntrys(apu.io.out.bits.apuEID).initOff.asUInt).bits.data := apu.io.out.bits.data
    assert(entrys(apu.io.out.bits.dbID).isWaitAPU)
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// ----------------------------------------------------- State Transfer ------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  entrys.zipWithIndex.foreach {
    case (e, i) =>
      val apuEID      = (djparam.nrDatBuf - 1 - i).U(apuIdBits - 1, 0)
      switch(e.state) {
        // FREE
        is(DBState.FREE) {
          val hit     = io.getDBID.fire & getDBIDId === i.U
          e           := 0.U.asTypeOf(e)
          e.state     := Mux(hit, DBState.ALLOC, e.state)
          assert(e.isFree | !hit)
          when(reqIsAtomic & hit) {
            assert(i.U >= nrDBWithoutAPUs.U)
            apuEntrys(apuEID).op              := io.getDBID.bits.atomicOp
            apuEntrys(apuEID).atomic.swapFst  := io.getDBID.bits.swapFst
          }
        }
        // ALLOC
        is(DBState.ALLOC) {
          val hit     = io.dbRCReqOpt.get.fire & io.dbRCReqOpt.get.bits.dbID === i.U
          val read    = io.dbRCReqOpt.get.bits.isRead & hit
          val clean   = io.dbRCReqOpt.get.bits.isClean & hit
          val exuAmo  = io.dbRCReqOpt.get.bits.exuAtomic
          e.state     := Mux(read, Mux(exuAmo, DBState.READ2APU, DBState.READ), Mux(clean, DBState.FREE, e.state))
        }
        // READ
        is(DBState.READ) {
          val hit     = io.dataFDB.fire & !hasReading & io.dataFDB.bits.dbID === i.U
          e.state     := Mux(hit, Mux(e.isLast, DBState.FREE, DBState.READING), e.state)
        }
        // READING
        is(DBState.READING) {
          val hit     = io.dataFDB.fire & io.dataFDB.bits.dbID === i.U
          val clean   = entrys(i).needClean
          e.state     := Mux(hit, Mux(clean, DBState.FREE, DBState.READ_DONE), e.state)
        }
        // READ_DONE
        is(DBState.READ_DONE) {
          val hit     = io.dbRCReqOpt.get.fire & io.dbRCReqOpt.get.bits.dbID === i.U
          val read    = io.dbRCReqOpt.get.bits.isRead & hit
          val clean   = io.dbRCReqOpt.get.bits.isClean & hit
          e.state     := Mux(read, DBState.READ, Mux(clean, DBState.FREE, e.state))
          e.rBeatNum  := 0.U
        }
        // READ2APU
        is(DBState.READ2APU) {
          val hit     = apu.io.in.valid & read2ApuDBID === i.U
          e.state     := Mux(hit, DBState.WAIT_APU, e.state)
          apuEntrys(apuEID).op := Mux(hit, AtomicOp.NONE, apuEntrys(apuEID).op)
        }
        // WAIT_APU
        is(DBState.WAIT_APU) {
          val hit     = apu.io.out.valid & apu.io.out.bits.dbID === i.U
          e.state     := Mux(hit , DBState.READ_DONE, e.state)
        }
      }
  }


// ----------------------------------------------------- Assertion ---------------------------------------------------------- //
  when(io.dataTDB.valid){ assert(entrys(io.dataTDB.bits.dbID).isAlloc) }

  val cntReg = RegInit(VecInit(Seq.fill(djparam.nrDatBuf) { 0.U(64.W) }))
  cntReg.zip(entrys).foreach { case (c, e) => c := Mux(e.isFree, 0.U, c + 1.U) }
  cntReg.zipWithIndex.foreach { case (c, i) => assert(c < TIMEOUT_DB.U, "DataBuffer ENTRY[0x%x] STATE[0x%x] TIMEOUT", i.U, entrys(i).state) }


// ----------------------------------------------------- Perf Counter ------------------------------------------------------- //
  require(djparam.nrDatBuf >= 4 & djparam.nrDatBuf % 4 == 0)
  for (i <- 0 until (djparam.nrDatBuf / 4)) {
    XSPerfAccumulate(s"pcu_dataBuf_group[${i}]_deal_req_cnt", io.getDBID.fire & (i * 4).U <= getDBIDId & getDBIDId <= (i * 4 + 3).U)
  }
  XSPerfAccumulate("pcu_dataBuf_deal_req_cnt", io.getDBID.fire)
  XSPerfAccumulate("pcu_dataBuf_req_retry_cnt", dbidRespQ.io.enq.fire & dbidRespQ.io.enq.bits.retry)

}
















