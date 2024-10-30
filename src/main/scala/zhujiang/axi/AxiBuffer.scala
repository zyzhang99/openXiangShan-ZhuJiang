package zhujiang.axi

import chisel3._
import chisel3.util._

class AxiBuffer(axiParams: AxiParams, depth:Int = 2) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new AxiBundle(axiParams))
    val out = new AxiBundle(axiParams)
  })
  io.out.aw <> Queue(io.in.aw, entries = depth, pipe = true)
  io.out.ar <> Queue(io.in.ar, entries = depth, pipe = true)
  io.out.w <> Queue(io.in.w, entries = depth, pipe = true)
  io.in.r <> Queue(io.out.r, entries = depth, pipe = true)
  io.in.b <> Queue(io.out.b, entries = depth, pipe = true)
}

object AxiBuffer {
  def apply(in: AxiBundle, depth:Int = 2, name:Option[String] = None) = {
    val buffer = Module(new AxiBuffer(in.params, depth))
    buffer.io.in <> in
    if(name.isDefined) buffer.suggestName(name.get)
    buffer.io.out
  }
}
