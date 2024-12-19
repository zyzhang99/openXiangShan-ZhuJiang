package dongjiang.pcu

import dongjiang._
import dongjiang.utils.FastArb
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import dongjiang.utils.FastArb._
import xs.utils.FastArbiter


class Xbar()(implicit p: Parameters) extends DJModule {
// ------------------------------------------ IO declaration ----------------------------------------------//
  val io = IO(new Bundle {
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
  def idSelDec2DecVec[T <: Bundle with HasToIncoID](in: DecoupledIO[T], out: Seq[DecoupledIO[T]]): Unit = {
    in.ready := false.B
    out.foreach(_.bits := in.bits)
    out.zipWithIndex.foreach {
      case (o, i) =>
        o.bits := in.bits
        val idMatch = WireInit(false.B)
        idMatch := in.bits.to === i.U
        when(idMatch) {
          o.valid := in.valid
          in.ready := o.ready
        }.otherwise {
          o.valid := false.B
        }
    }
  }


  // in ---> [queue0] --->  [redirects] ---> [queue1] ---> [arbiter] ---> [queue2] ---> out
  def interConnect[T <: Bundle with HasToIncoID](in: Seq[DecoupledIO[T]], out: Seq[DecoupledIO[T]], q0: Int = 0, q0_pipe: Boolean = true, q1: Int = 0, q1_pipe: Boolean = true, q2: Int = 0, q2_pipe: Boolean = true): Unit = {
    val redirects = Seq.fill(in.size) { Seq.fill(out.size) { WireInit(0.U.asTypeOf(in(0))) } }
    in.zipWithIndex.foreach { case (m, i) => idSelDec2DecVec(Queue(m, entries = q0, pipe = q0_pipe), redirects(i)) }
    out.zipWithIndex.foreach { case (m, i) => m <> Queue(fastArbDec(redirects.map { case a => Queue(a(i), entries = q1, pipe = q1_pipe) }), q2, pipe = q2_pipe) }
  }

  // There is a lot of room for optimization of the connection
  interConnect(in = io.req2Exu.in,                  out = io.req2Exu.out, q0 = 2, q0_pipe = false) // Adding queues for timing considerations

  interConnect(in = io.reqAck2Intf.in,              out = io.reqAck2Intf.out)

  interConnect(in = io.resp2Intf.in,                out = io.resp2Intf.out, q0 = 2, q0_pipe = false) // Adding queues for timing considerations

  interConnect(in = io.req2Intf.in,                 out = io.req2Intf.out, q0 = 2, q0_pipe = false) // Adding queues for timing considerations

  interConnect(in = io.resp2Exu.in,                 out = io.resp2Exu.out)

  io.dbSigs.out(0).dbRCReq                          <> Queue(fastArbDec(io.dbSigs.in0), entries = 2) // Adding queues for timing considerations

  io.dbSigs.out(0).getDBID                          <> fastArbDec(io.dbSigs.in1.map(_.getDBID))

  interConnect(in = io.dbSigs.out.map(_.dbidResp),  out = io.dbSigs.in1.map(_.dbidResp))

  interConnect(in = io.dbSigs.out.map(_.dataFDB),   out = io.dbSigs.in1.map(_.dataFDB), q0 = 1) // Adding queues for timing considerations

  io.dbSigs.out(0).dataTDB                          <> fastArbDec(io.dbSigs.in1.map(_.dataTDB))

}