package zhujiang.device.bridge.axi

import chisel3._
import chisel3.util._

class DataBufferAllocReq(outstanding: Int) extends Bundle {
  val idxOH = UInt(outstanding.W)
  val size = UInt(3.W)
  val waitNum = UInt(log2Ceil(outstanding).W)
}

class SelNto1(size:Int, outstanding: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(size, Flipped(Decoupled(new DataBufferAllocReq(outstanding))))
    val out = Decoupled(new DataBufferAllocReq(outstanding))
  })

  private val onlyOne = PopCount(io.in.map(_.valid)) === 1.U
  private val oldestOHMatrix = io.in.zipWithIndex.map({ case (self, idx) =>
    io.in.zipWithIndex.filterNot(_._2 == idx).map(i => (i._1.valid && self.valid && (self.bits.waitNum <= i._1.bits.waitNum)) ^ i._1.valid)
  })
  private val oldestOHSeq = oldestOHMatrix.map(_.reduce(_ | _)).map(!_)
  private val oldestOH = PriorityEncoderOH(Cat(oldestOHSeq.reverse))
  private val defaultValue = Cat(io.in.map(_.valid).reverse)
  private val selOH = Mux(onlyOne, defaultValue, oldestOH)

  io.out.valid := io.in.map(_.valid).reduce(_ | _)
  io.out.bits := Mux1H(selOH, io.in.map(_.bits))

  for(i <- io.in.indices) {
    io.in(i).ready := io.out.ready && selOH(i)
    when(io.out.valid && !selOH(i) && io.in(i).valid) {
      assert(io.out.bits.waitNum <= io.in(i).bits.waitNum)
    }
  }
  when(io.out.valid) {
    assert(PopCount(selOH) === 1.U)
  }
}

class DataBufferAllocReqSelector(outstanding: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(outstanding, Flipped(Decoupled(new DataBufferAllocReq(outstanding))))
    val out = Decoupled(new AxiDataBufferAllocReq(outstanding))
  })

  private val selPipe = Module(new Queue(new AxiDataBufferAllocReq(outstanding), entries = 2))
  private val selector = Module(new SelNto1(outstanding, outstanding))
  selector.io.in.zip(io.in).foreach({case(a, b) => a <> b})
  selPipe.io.enq.valid := selector.io.out.valid
  selPipe.io.enq.bits.idxOH := selector.io.out.bits.idxOH
  selPipe.io.enq.bits.size := selector.io.out.bits.size
  selector.io.out.ready := selPipe.io.enq.ready
  io.out <> selPipe.io.deq
}
