package dongjiang.pcu

import dongjiang._
import zhujiang.chi._
import xijiang.Node
import dongjiang.chi._
import dongjiang.pcu._
import dongjiang.pcu.exu._
import dongjiang.pcu.intf._
import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import dongjiang.utils.FastArb._
import xijiang.router.base.DeviceIcnBundle
import zhujiang.HasZJParams


/*
 * System Architecture: (2 RNSLAVE, 1 RNMASTER, 1 SNMASTER, 1 DataBuffer and 2 EXU)
 *
 *                                          -----------------------------------------------------------------
 *                                          |                       |       Dir       |                     |
 *               ------------               |      -----------      -------------------      ---------      |                ------------
 *     CSN <---> | RNSLAVE  | <---> | <---> | ---> |  MSHR   | ---> | ProcessPipe * 2 | ---> | Queue | ---> |  <---> | <---> | RNMASTER | <---> CSN
 *               ------------       |       |      -----------      -------------------      ---------      |        |       ------------
 *                                  |       |                                |                              |        |
 *                                  |       -----------------------------------------------------------------        |
 *                                  |                                        |                                       |
 *                                  |                                 --------------                                 |
 *                                 XBar <-------------------------->  | DataBuffer | <----------------------------> XBar
 *                                  |                                 --------------                                 |
 *                                  |                                        |                                       |
 *                                  |       -----------------------------------------------------------------        |
 *                                  |       |                                |                              |        |
 *               ------------       |       |      -----------      -------------------      ---------      |        |       ------------
 *   Local <---> | RNSLAVE  | <---> | <---> | ---> |  MSHR   | ---> | ProcessPipe * 2 | ---> | Queue | ---> |  <---> | <---> | SNMASTER | <---> Local
 *               ------------               |      -----------      -------------------      ---------      |                ------------
 *                                          |                       |       Dir       |                     |
 *                                          -----------------------------------------------------------------
 */

@instantiable
class ProtocolCtrlUnit(localHf: Node, csnRf: Option[Node] = None, csnHf: Option[Node] = None)(implicit p: Parameters) extends DJRawModule
  with ImplicitClock with ImplicitReset {
  // ------------------------------------------ IO declaration ----------------------------------------------//
  @public val io  = IO(new Bundle {
    val hnfID         = Input(UInt(fullNodeIdBits.W))
    val pcuID         = Input(UInt(pcuBankBits.W)) // PCU Bank ID
    val dcuNodeIDVec  = Input(Vec(nrBankPerPCU, UInt(fullNodeIdBits.W))) // DCU Friend Node ID Vec
    val toLocal       = new DeviceIcnBundle(localHf)
    val toCSNOpt      = if(hasCSN) Some(new Bundle {
      val hn          = new DeviceIcnBundle(csnHf.get)
      val rn          = new DeviceIcnBundle(csnRf.get)
    }) else None
  })
  @public val reset   = IO(Input(AsyncReset()))
  @public val clock   = IO(Input(Clock()))
  val implicitClock   = clock
  val implicitReset   = reset

// ------------------------------------------ Modules declaration ----------------------------------------------//
  // interfaces
  val localRnSlave    = Module(new RnSlaveIntf(djparam.localRnSlaveIntf, localHf))
  val localSnMaster   = Module(new SnMasterIntf(djparam.localSnMasterIntf, localHf))
  val csnRnSlaveOpt   = if (hasCSN) Some(Module(new RnSlaveIntf(djparam.csnRnSlaveIntf.get,csnHf.get))) else None
  val csnRnMasterOpt  = if (hasCSN) Some(Module(new RnMasterIntf(djparam.csnRnMasterIntf.get, csnRf.get))) else None
  val intfs           = if (hasCSN) Seq(localRnSlave, localSnMaster, csnRnSlaveOpt.get, csnRnMasterOpt.get)
                        else        Seq(localRnSlave, localSnMaster)
  // data buffer
  val databuffer      = Module(new DataBuffer())
  // xbar
  val xbar            = Module(new Xbar())
  // EXUs
  val exus            = Seq.fill(nrBankPerPCU) { Module(new ExecuteUnit()) }



// ---------------------------------------------- Connection ---------------------------------------------------//
  /*
   * Connect LOCAL RING CHI IO
   */
  localSnMaster.io.chi                    <> DontCare
  localRnSlave.io.chi                     <> DontCare

  // rx req
  io.toLocal.rx.req.get                   <> localRnSlave.io.chi.rx.req.get
  
  // rx rsp
  localSnMaster.io.chi.rx.resp.get.valid  := io.toLocal.rx.resp.get.valid & fromSnNode(io.toLocal.rx.resp.get.bits.asTypeOf(new RespFlit()).SrcID)
  localRnSlave.io.chi.rx.resp.get.valid   := io.toLocal.rx.resp.get.valid & fromCcNode(io.toLocal.rx.resp.get.bits.asTypeOf(new RespFlit()).SrcID)
  localSnMaster.io.chi.rx.resp.get.bits   := io.toLocal.rx.resp.get.bits
  localRnSlave.io.chi.rx.resp.get.bits    := io.toLocal.rx.resp.get.bits
  io.toLocal.rx.resp.get.ready            := (localSnMaster.io.chi.rx.resp.get.ready & fromSnNode(io.toLocal.rx.resp.get.bits.asTypeOf(new RespFlit()).SrcID)) |
                                             (localRnSlave.io.chi.rx.resp.get.ready  & fromCcNode(io.toLocal.rx.resp.get.bits.asTypeOf(new RespFlit()).SrcID))
  // rx data
  // TODO: can be optimize by data directly send to data buffer
  localSnMaster.io.chi.rx.data.get.valid  := io.toLocal.rx.data.get.valid & fromSnNode(io.toLocal.rx.data.get.bits.asTypeOf(new DataFlit()).SrcID)
  localRnSlave.io.chi.rx.data.get.valid   := io.toLocal.rx.data.get.valid & fromCcNode(io.toLocal.rx.data.get.bits.asTypeOf(new DataFlit()).SrcID)
  localSnMaster.io.chi.rx.data.get.bits   := io.toLocal.rx.data.get.bits
  localRnSlave.io.chi.rx.data.get.bits    := io.toLocal.rx.data.get.bits
  io.toLocal.rx.data.get.ready            := (localSnMaster.io.chi.rx.data.get.ready & fromSnNode(io.toLocal.rx.data.get.bits.asTypeOf(new DataFlit()).SrcID)) |
                                             (localRnSlave.io.chi.rx.data.get.ready & fromCcNode(io.toLocal.rx.data.get.bits.asTypeOf(new DataFlit()).SrcID))

  // tx req
  io.toLocal.tx.req.get                   <> localSnMaster.io.chi.tx.req.get

  // tx snoop
  io.toLocal.tx.snoop.get                 <> localRnSlave.io.chi.tx.snoop.get

  // tx resp
  io.toLocal.tx.resp.get                  <> localRnSlave.io.chi.tx.resp.get

  // tx data
  io.toLocal.tx.data.get.valid            := localSnMaster.io.chi.tx.data.get.valid | localRnSlave.io.chi.tx.data.get.valid
  io.toLocal.tx.data.get.bits             := Mux(localSnMaster.io.chi.tx.data.get.valid, localSnMaster.io.chi.tx.data.get.bits, localRnSlave.io.chi.tx.data.get.bits)
  localSnMaster.io.chi.tx.data.get.ready  := io.toLocal.tx.data.get.ready
  localRnSlave.io.chi.tx.data.get.ready   := io.toLocal.tx.data.get.ready & !localSnMaster.io.chi.tx.data.get.valid


  /*
   * Connect CSN CHI IO
   */
  if(hasCSN) {
    io.toCSNOpt.get.hn <> csnRnSlaveOpt.get.io.chi
    io.toCSNOpt.get.rn <> csnRnMasterOpt.get.io.chi
  }

  // TODO: Xbar -> Ring

  /*
   * Connect Intf <-> Xbar
   */
  intfs.zipWithIndex.foreach {
    case(intf, i) =>
      intf.io.hnfID                           := io.hnfID
      intf.io.pcuID                           := io.pcuID
      intf.io.fIDVec                          := io.dcuNodeIDVec
      // EXU ctrl signals
      if (intf.io.req2ExuOpt.nonEmpty) {
          xbar.io.req2Exu.in(i)               <> intf.io.req2ExuOpt.get
      } else {
          xbar.io.req2Exu.in(i)               <> DontCare
      }
      if (intf.io.reqAck2IntfOpt.nonEmpty) {
          xbar.io.reqAck2Intf.out(i)          <> intf.io.reqAck2IntfOpt.get
      } else {
          xbar.io.reqAck2Intf.out(i)          <> DontCare
      }
      if (intf.io.resp2IntfOpt.nonEmpty) {
          xbar.io.resp2Intf.out(i)            <> intf.io.resp2IntfOpt.get
      } else {
          xbar.io.resp2Intf.out(i)            <> DontCare
      }
      xbar.io.req2Intf.out(i)                 <> intf.io.req2Intf
      xbar.io.resp2Exu.in(i)                  <> intf.io.resp2Exu
      // Intf DataBuffer signals
      if (intf.io.dbSigs.dbRCReqOpt.nonEmpty) {
          xbar.io.dbSigs.in0(i)               <> intf.io.dbSigs.dbRCReqOpt.get
      } else {
          xbar.io.dbSigs.in0(i)               <> DontCare
      }
      xbar.io.dbSigs.in1(i).getDBID           <> intf.io.dbSigs.getDBID
      xbar.io.dbSigs.in1(i).dbidResp          <> intf.io.dbSigs.dbidResp
      xbar.io.dbSigs.in1(i).dataFDB           <> intf.io.dbSigs.dataFDB
      xbar.io.dbSigs.in1(i).dataTDB           <> intf.io.dbSigs.dataTDB
  }

  /*
   * Connect EXUs <-> Xbar
   */
  exus.zipWithIndex.foreach {
    case (exu, i) =>
      exu.io.dcuID                    := i.U
      exu.io.pcuID                    := io.pcuID
      // TODO: Lower Power Ctrl
      exu.io.valid                    := true.B
      // slice ctrl signals
      xbar.io.req2Exu.out(i)          <> exu.io.req2Exu
      xbar.io.reqAck2Intf.in(i)       <> exu.io.reqAck2Intf
      xbar.io.resp2Intf.in(i)         <> exu.io.resp2Intf
      xbar.io.req2Intf.in(i)          <> exu.io.req2Intf
      xbar.io.resp2Exu.out(i)         <> exu.io.resp2Exu
      // slice DataBuffer signals
      xbar.io.dbSigs.in0(i+nrIntf)    <> exu.io.dbRCReq
  }

  /*
   * Connect DataBuffer <-> Xbar
   */
  databuffer.io <> xbar.io.dbSigs.out(0)

}
