package dongjiang.pcu.intf

import zhujiang.chi.ReqOpcode._
import zhujiang.chi.RspOpcode._
import zhujiang.chi.DatOpcode._
import zhujiang.chi.SnpOpcode._
import dongjiang._
import dongjiang.pcu._
import dongjiang.chi._
import chisel3._
import org.chipsalliance.cde.config._
import chisel3.util.{Cat, Decoupled, PopCount, RegEnable, Valid, ValidIO, log2Ceil}
import xijiang.Node


class RnMasterIntf(param: InterfaceParam, node: Node)(implicit p: Parameters) extends IntfBaseIO(param, node) {
  // Del it
  io <> DontCare
  dontTouch(io)

}