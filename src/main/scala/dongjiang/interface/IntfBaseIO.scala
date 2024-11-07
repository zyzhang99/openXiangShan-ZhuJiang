package dongjiang.pcu.intf

import dongjiang._
import dongjiang.pcu._
import dongjiang.chi._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils._
import dongjiang.utils.FastArb._
import xijiang.router.base.DeviceIcnBundle
import xijiang.Node
import xs.utils.perf.DebugOptionsKey

abstract class IntfBaseIO(param: InterfaceParam, node: Node)(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val hnfID           = Input(UInt(useNodeIdBits.W))
    val pcuID           = Input(UInt(pcuBankBits.W))
    val fIDVec          = Input(Vec(nrBankPerPCU, UInt(fullNodeIdBits.W))) // DCU Friends Node ID Vec
    // To CHI Signals
    val chi             = new DeviceIcnBundle(node)
    // To EXU Signals
    val req2ExuOpt      = if(param.hasReq2exu) Some(Decoupled(new Req2ExuBundle())) else None
    val reqAck2IntfOpt  = if(param.hasReq2exu) Some(Flipped(Decoupled(new ReqAck2IntfBundle()))) else None
    val resp2IntfOpt    = if(param.hasReq2exu) Some(Flipped(Decoupled(new Resp2IntfBundle()))) else None
    val req2Intf        = Flipped(Decoupled(new Req2IntfBundle()))
    val resp2Exu        = Decoupled(new Resp2ExuBundle())
    // To DataBuffer Signals
    val dbSigs          = new DBBundle(param.hasDBRCReq)

    // for debug, only use in SnMaster
    val chi_tx_req_bits_DbgAddr = if (p(DebugOptionsKey).EnableDebug) { Some(Output(UInt(fullAddrBits.W))) } else None


    def req2Exu         = req2ExuOpt.get
    def reqAck2Intf     = reqAck2IntfOpt.get
    def resp2Intf       = resp2IntfOpt.get
  })
}