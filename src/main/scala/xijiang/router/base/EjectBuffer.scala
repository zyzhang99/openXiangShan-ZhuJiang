package xijiang.router.base

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xs.utils.PickOneLow
import zhujiang.ZJModule
import zhujiang.chi.Flit

class EjectBuffer[T <: Flit](gen: T, size: Int, chn: String)(implicit p: Parameters) extends ZJModule {
  private val tagBits = 12 + niw // SrcId + TxnId
  require(size >= 3)
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(gen))
    val deq = Decoupled(gen)
  })
  override val desiredName = s"EjectBuffer$chn"
  private val oqueue = Module(new Queue(gen, size - 1))
  private val ipipe = Module(new Queue(gen, 1, pipe = true) )
  private val rsvdTags = Reg(Vec(size, UInt(tagBits.W)))
  private val rsvdValids = RegInit(VecInit(Seq.fill(size)(false.B)))
  private val empties = RegInit(size.U(log2Ceil(size + 1).W))
  private val rsvdNumReg = RegInit(0.U(log2Ceil(size + 1).W))

  oqueue.io.enq <> ipipe.io.deq
  io.deq <> oqueue.io.deq
  private val enqFire = ipipe.io.enq.fire
  private val deqFire = oqueue.io.deq.fire
  when(enqFire && !deqFire) {
    assert(empties > 0.U)
    empties := empties - 1.U
  }.elsewhen(!enqFire && deqFire) {
    assert(empties < size.U)
    empties := empties + 1.U
  }
  when(empties.orR) {
    assert(ipipe.io.enq.ready)
  }

  private def getTag(flit: Flit): UInt = Cat(flit.src, flit.txn)

  private val rsvdSel = PickOneLow(rsvdValids)
  private val rsvdHit = Cat(rsvdTags.zip(rsvdValids).map(e => e._1 === getTag(io.enq.bits) && e._2)).orR
  private val doReserve = io.enq.valid && !io.enq.ready && !rsvdHit && rsvdSel.valid

  when(rsvdHit && ipipe.io.enq.fire) {
    rsvdNumReg := rsvdNumReg - 1.U
  }.elsewhen(doReserve) {
    rsvdNumReg := rsvdNumReg + 1.U
  }
  assert(rsvdNumReg === PopCount(rsvdValids))

  for(idx <- rsvdValids.indices) {
    val v = rsvdValids(idx)
    val tag = rsvdTags(idx)
    val doRsv = rsvdSel.bits(idx) && doReserve
    when(v && ipipe.io.enq.fire && getTag(io.enq.bits) === tag) {
      v := false.B
    }.elsewhen(doRsv) {
      v := true.B
    }
    when(doRsv) {
      tag := getTag(io.enq.bits)
    }
  }

  private val allowEnq = Mux(rsvdHit, true.B, rsvdNumReg < empties)
  ipipe.io.enq.valid := io.enq.valid & allowEnq
  ipipe.io.enq.bits := io.enq.bits
  io.enq.ready := ipipe.io.enq.ready & allowEnq
}
