package zhujiang.tilelink

import chisel3._
import chisel3.util._

class TLUBuffer(axiParams: TilelinkParams, depth:Int = 2) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new TLULBundle(axiParams))
    val out = new TLULBundle(axiParams)
  })
  io.out.a <> Queue(io.in.a, entries = depth, pipe = true)
  io.in.d <> Queue(io.out.d, entries = depth, pipe = true)
}

object TLUBuffer {
  def apply(in:TLULBundle, depth:Int = 2, name:Option[String] = None):TLULBundle = {
    val buffer = Module(new TLUBuffer(in.params, depth))
    buffer.io.in <> in
    if(name.isDefined) buffer.suggestName(name.get)
    buffer.io.out
  }
}