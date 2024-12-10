package dongjiang.pcu

import dongjiang._
import AtomicOp._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import dongjiang.utils.Encoder.RREncoder
import xs.utils.perf.HasPerfLogging


object AtomicOp {
  val width = 4
  val LDADD   = 0x0.U
  val LDCLR   = 0x1.U
  val LDEOR   = 0x2.U
  val LDSET   = 0x3.U
  val LDSMAX  = 0x4.U
  val LDSMIN  = 0x5.U
  val LDUMAX  = 0x6.U
  val LDUMIN  = 0x7.U
  val SWAP    = 0x8.U
  val COMPARE = 0x9.U
  val NONE    = 0xF.U
}


class AtomicDataBundle(implicit p: Parameters) extends DJBundle {
  val data    = UInt(256.W)
  val mask    = UInt(32.W)
  val swapFst = Bool() // Only use in compare
}


class APUEntry(implicit p: Parameters) extends DJBundle {
  val op          = UInt(AtomicOp.width.W)
  val atomic      = new AtomicDataBundle()
  val initOff     = Bool()
}

/*
 * !!!!!!!!! This is a highly customizable module for CHI only !!!!!!!!!
 */
class AtomicProcessUnit()(implicit p: Parameters) extends DJModule with HasPerfLogging {
// ------------------------------------------ IO declaration --------------------------------------------- //
  val io = IO(new DJBundle {
    val in            = Flipped(Valid(new DJBundle with HasDBID {
      val op          = UInt(AtomicOp.width.W)
      val data        = UInt(256.W)
      val atomic      = new AtomicDataBundle()
    }))
    val out           = Valid(new DJBundle with HasDBID {
      val data        = UInt(256.W)
    })
  })

  // assert
  assert(io.in.bits.op <= COMPARE | !io.in.valid)
  assert(Mux(io.in.bits.op === COMPARE, PopCount(io.in.bits.atomic.mask) <= 32.U, PopCount(io.in.bits.atomic.mask) <= 8.U) | !io.in.valid)


// ----------------------------------------- Reg and Wire declaration ------------------------------------ //
  val op          = io.in.bits.op
  // Use In Parse
  val amoDataVec  = Wire(Vec(32, UInt(8.W)))
  val inDataVec   = Wire(Vec(32, UInt(8.W)))
  val loadDataVec = Wire(Vec(8,  UInt(8.W)))
  val swapDataVec = Wire(Vec(16, UInt(8.W)))
  val compDataVec = Wire(Vec(16, UInt(8.W)))
  val initDataVec = Wire(Vec(16, UInt(8.W)))
  val outDataVec  = Wire(Vec(32, UInt(8.W)))
  // Use In Execute
  val dealData    = WireInit(0.U(128.W))
  // Use In Out
  val firstIdx    = WireInit(31.U)
  val lastIdx     = WireInit(31.U)
  val outReg      = RegInit(0.U.asTypeOf(io.out))




// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------------- Parse Data Input ------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Parse Data Input
   */
  val firstByte     = PriorityEncoder(io.in.bits.atomic.mask)
  val bytesNum      = PopCount(io.in.bits.atomic.mask).asUInt
  val halfBytesNum  = (bytesNum >> 1).asUInt
  amoDataVec        := io.in.bits.atomic.data.asTypeOf(amoDataVec)
  inDataVec         := io.in.bits.data.asTypeOf(inDataVec)
  assert(firstByte < 16.U | !io.in.valid)

  /*
   * Parse loadData
   */
  loadDataVec.zipWithIndex.foreach {
    case(load, i) =>
      load := Mux(i.U < bytesNum, amoDataVec(i.U + firstByte), 0.U)
  }

  /*
   * Parse swapData
   */
  swapDataVec.zipWithIndex.foreach {
    case(swap, i) =>
      when(op === SWAP) {
        swap := Mux(i.U < bytesNum, amoDataVec(i.U + firstByte), 0.U)
      }.elsewhen(io.in.bits.atomic.swapFst) {
        swap := Mux(i.U < halfBytesNum, amoDataVec(i.U + firstByte), 0.U)
      }.otherwise {
        swap := Mux(i.U < halfBytesNum, amoDataVec(i.U + firstByte + halfBytesNum), 0.U)
      }
  }

  /*
   * Parse compData
   */
  compDataVec.zipWithIndex.foreach {
    case (comp, i) =>
     when(io.in.bits.atomic.swapFst) {
        comp := Mux(i.U < halfBytesNum, amoDataVec(i.U + firstByte + halfBytesNum), 0.U)
      }.otherwise {
        comp := Mux(i.U < halfBytesNum, amoDataVec(i.U + firstByte), 0.U)
      }
  }

  /*
   * Parse initData
   */
  initDataVec.zipWithIndex.foreach {
    case (init, i) =>
      when(op === COMPARE & io.in.bits.atomic.swapFst) {
        init := Mux(i.U < halfBytesNum, inDataVec(i.U + firstByte + halfBytesNum), 0.U)
      }.elsewhen(op === COMPARE) {
        init := Mux(i.U < halfBytesNum, inDataVec(i.U + firstByte), 0.U)
      }.otherwise {
        init := Mux(i.U < bytesNum,     inDataVec(i.U + firstByte), 0.U)
      }
  }



// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------------- Excute Atomic ---------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Get the absolute value
   */
  val sIntBit   = ((bytesNum << 3.U).asUInt - 1.U).asTypeOf(UInt(6.W))
  def getAbs(in: UInt): UInt = {
    val outVec  = Wire(Vec(64, Bool()))
    when(in(sIntBit)){
      val temp  = ~(in - 1.U)
      temp.asBools.zipWithIndex.foreach { case(t, i) => outVec(i) := Mux(i.U < sIntBit, t, false.B) }
    }.otherwise {
      outVec    := in.asBools
    }
    outVec.asUInt
  }

  /*
   * AtomicLoad
   */
  when(op < SWAP) {
    // Get txnData and initData
    val txnData       = loadDataVec.asTypeOf(UInt(64.W))
    val initData      = initDataVec.asTypeOf(txnData)
    // Get
    switch(op) {
      // LDADD
      is(LDADD) {
        dealData      := txnData + initData
      }
      // LDCLR
      is(LDCLR) {
        dealData      := initData & (~txnData).asUInt
      }
      // LDEOR
      is(LDEOR) {
        dealData      := initData ^ txnData
      }
      // LDSET
      is(LDSET) {
        dealData      := initData | txnData
      }
      // LDSMAX
      is(LDSMAX) {
        val txnBigger = Wire(Bool())
        when(txnData(sIntBit) === initData(sIntBit)) {
          val txnAbs  = Mux(txnData(sIntBit), getAbs(txnData), txnData)
          val initAbs = Mux(txnData(sIntBit), getAbs(initData), initData)
          txnBigger   := txnAbs > initAbs
        }.otherwise {
          txnBigger   := !txnData(sIntBit)
        }
        dealData      := Mux(txnBigger, txnData, initData)
      }
      // LDSMIN
      is(LDSMIN) {
        val iniBigger = Wire(Bool())
        when(txnData(sIntBit) === initData(sIntBit)) {
          val txnAbs  = Mux(txnData(sIntBit), getAbs(txnData), txnData)
          val initAbs = Mux(txnData(sIntBit), getAbs(initData), initData)
          iniBigger   := txnAbs < initAbs
        }.otherwise {
          iniBigger   := txnData(sIntBit)
        }
        dealData      := Mux(iniBigger, txnData, initData)
      }
      // LDUMAX
      is(LDUMAX) {
        dealData      := Mux(txnData > initData, txnData, initData)
      }
      // LDUMIN
      is(LDUMIN) {
        dealData      := Mux(txnData < initData, txnData, initData)
      }
    }
  /*
   * AtomicSwap
   */
  }.elsewhen(op === SWAP) {
    val swapData      = swapDataVec.asTypeOf(UInt(64.W))
    dealData          := swapData
  /*
   * AtomicCompare
   */
  }.elsewhen(op === COMPARE) {
    val swapData      = swapDataVec.asTypeOf(UInt(128.W))
    val compData      = compDataVec.asTypeOf(UInt(128.W))
    val initData      = initDataVec.asTypeOf(compData)
    dealData          := Mux(compData === initData, swapData, compData)
  /*
   * Other
   */
  }.otherwise {
    assert(false.B | !io.in.valid)
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------------- Output Result ---------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  val dealDataVec   = dealData.asTypeOf(Vec(16, UInt(8.W)))

  outDataVec.zipWithIndex.foreach {
    case(out, i) =>
      when(op === COMPARE & io.in.bits.atomic.swapFst) {
        firstIdx    := firstByte + halfBytesNum
        lastIdx     := firstByte + bytesNum
      }.elsewhen(op === COMPARE) {
        firstIdx    := firstByte
        lastIdx     := firstByte + halfBytesNum
      }.otherwise {
        firstIdx    := firstByte
        lastIdx     := firstByte + bytesNum
      }
      out := Mux(firstIdx <= i.U & i.U < lastIdx, dealDataVec(i.U - firstIdx), inDataVec(i))
  }

  outReg.valid      := io.in.valid
  outReg.bits.dbID  := io.in.bits.dbID
  outReg.bits.data  := outDataVec.asUInt

  io.out            := outReg
}
















