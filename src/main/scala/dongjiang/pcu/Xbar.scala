package dongjiang.pcu

import dongjiang._
import dongjiang.utils.FastArb
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import dongjiang.utils.FastArb._
import xs.utils.FastArbiter

class IdMap(implicit p: Parameters) extends DJModule {
  val io = IO(new Bundle {
    val bankIDVec = Input(Vec(nrBankPerPCU, UInt(bankBits.W)))
    val inBank    = Input(UInt(bankBits.W))
    val to        = Output(UInt(IncoID.width.W))
  })
  val to = WireInit(0.U(IncoID.width.W))
  io.bankIDVec.zipWithIndex.foreach { case (id, i) =>  when(id === io.inBank) { to := i.U } }
  io.to := to
}




class Xbar()(implicit p: Parameters) extends DJModule {
// ------------------------------------------ IO declaration ----------------------------------------------//
  val io = IO(new Bundle {
    val bankIDVec   = Input(Vec(nrBankPerPCU, UInt(bankBits.W)))
    // Intf ctrl signals
    val req2Exu     = new Bundle {
      val in        = Vec(nrIntf, Flipped(Decoupled(new Req2ExuBundle()))) // expect SNMaster
      val out       = Vec(nrBankPerPCU, Decoupled(new Req2ExuBundle()))
    }
    val reqAck2Intf = new Bundle {
      val in        = Vec(nrBankPerPCU, Flipped(Decoupled(new ReqAck2IntfBundle()))) // expect SNMaster
      val out       = Vec(nrIntf, Decoupled(new ReqAck2IntfBundle()))
    }
    val resp2Intf   = new Bundle {
      val in        = Vec(nrBankPerPCU, Flipped(Decoupled(new Resp2IntfBundle()))) // expect SNMaster
      val out       = Vec(nrIntf, Decoupled(new Resp2IntfBundle()))
    }
    val req2Intf    = new Bundle {
      val in        = Vec(nrBankPerPCU, Flipped(Decoupled(new Req2IntfBundle())))
      val out       = Vec(nrIntf, Decoupled(new Req2IntfBundle()))
    }
    val resp2Exu    = new Bundle {
      val in        = Vec(nrIntf, Flipped(Decoupled(new Resp2ExuBundle())))
      val out       = Vec(nrBankPerPCU, Decoupled(new Resp2ExuBundle()))
    }
    // slice DataBuffer signals
    val dbSigs      = new Bundle {
      val in0       = Vec(nrBankPerPCU + nrIntf, Flipped(Decoupled(new DBRCReq())))
      val in1       = Vec(nrIntf, Flipped(new DBBundle(hasDBRCReq = false)))
      val out       = Vec(1, new DBBundle(hasDBRCReq = true))
    }
  })


// ------------------------------------------ Modules declaration And Connection ----------------------------------------------//
  val req2ExuIdMaps = Seq.fill(nrIntf) { Module(new IdMap()) }

  // --------------------- Wire declaration ------------------------//
  val req2ExuReMap  = Wire(Vec(nrIntf, Decoupled(new Req2ExuBundle())))

  // --------------------- Connection ------------------------//
  // req2Exu bank ReMap
  req2ExuReMap.zip(io.req2Exu.in).foreach{ case(reMap, in) => reMap <> in }
  req2ExuIdMaps.zipWithIndex.foreach {
    case(m, i) =>
      m.io.bankIDVec := io.bankIDVec
      m.io.inBank := io.req2Exu.in(i).bits.pcuIndex.bankID
      req2ExuReMap(i).bits.pcuIndex.to.incoID := m.io.to
      // assert
      assert(io.bankIDVec.map(_ === io.req2Exu.in(i).bits.pcuIndex.bankID).reduce(_ | _) | !io.req2Exu.in(i).valid)
  }

  def idSelDec2DecVec[T <: Bundle](in: DecoupledIO[T], incoID: UInt, out: Seq[DecoupledIO[T]]): Unit = {
    in.ready := false.B
    out.foreach(_.bits := in.bits)
    out.zipWithIndex.foreach {
      case (o, i) =>
        o.bits := in.bits
        val idMatch = WireInit(false.B)
        idMatch := incoID === i.U
        when(idMatch) {
          o.valid := in.valid
          in.ready := o.ready
        }.otherwise {
          o.valid := false.B
        }
    }
  }


  // in --->  [redirects] ---> [queue0] ---> [arbiter] ---> [queue1] ---> out
  def interConnect[T <: Bundle](in: Seq[DecoupledIO[T]], incoID: Seq[UInt], q1: Int, q2: Int, out: Seq[DecoupledIO[T]]): Unit = {
    val redirects = Seq.fill(in.size) { Seq.fill(out.size) { WireInit(0.U.asTypeOf(in(0))) } }
    in.zipWithIndex.foreach { case (m, i) => idSelDec2DecVec(m, incoID(i), redirects(i)) }
    out.zipWithIndex.foreach { case (m, i) => m <> Queue(fastArbDec(redirects.map { case a => Queue(a(i), entries = q1, pipe = true) }), q2, pipe = true) }
  }

  // There is a lot of room for optimization of the connection
  interConnect(in = req2ExuReMap,       incoID = req2ExuReMap.map(_.bits.pcuIndex.to.incoID),     q1 = 0, q2 = 0, out = io.req2Exu.out)

  interConnect(in = io.reqAck2Intf.in,  incoID = io.reqAck2Intf.in.map(_.bits.to.incoID),         q1 = 0, q2 = 0, out = io.reqAck2Intf.out)

  interConnect(in = io.resp2Intf.in,    incoID = io.resp2Intf.in.map(_.bits.pcuIndex.to.incoID),  q1 = 0, q2 = 0, out = io.resp2Intf.out)

  interConnect(in = io.req2Intf.in,     incoID = io.req2Intf.in.map(_.bits.pcuIndex.to.incoID),   q1 = 0, q2 = 0, out = io.req2Intf.out)

  interConnect(in = io.resp2Exu.in,     incoID = io.resp2Exu.in.map(_.bits.pcuIndex.to.incoID),   q1 = 0, q2 = 0, out = io.resp2Exu.out)

  io.dbSigs.out(0).dbRCReq <> fastArbDec(io.dbSigs.in0)

  io.dbSigs.out(0).getDBID <> fastArbDec(io.dbSigs.in1.map(_.getDBID))

  interConnect(in = io.dbSigs.out.map(_.dbidResp),  incoID = io.dbSigs.out.map(_.dbidResp).map(_.bits.to.incoID), q1 = 0, q2 = 0, out = io.dbSigs.in1.map(_.dbidResp))

  interConnect(in = io.dbSigs.out.map(_.dataFDB),   incoID = io.dbSigs.out.map(_.dataFDB).map(_.bits.to.incoID),  q1 = 0, q2 = 0, out = io.dbSigs.in1.map(_.dataFDB))

  io.dbSigs.out(0).dataTDB <> fastArbDec(io.dbSigs.in1.map(_.dataTDB))

}