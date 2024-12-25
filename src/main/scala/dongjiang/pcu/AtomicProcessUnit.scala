package dongjiang.pcu

import dongjiang._
import AtomicOp._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
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
  // S1: Receive IO In
  val valid_s1_g      = RegNext(io.in.valid)
  val in_s1_g         = RegEnable(io.in.bits, io.in.valid)
  // S1: Use In Parse
  val op_s1           = in_s1_g.op
  val amoDataVec_s1   = Wire(Vec(32, UInt(8.W)))
  val inDataVec_s1    = Wire(Vec(32, UInt(8.W)))
  val loadDataVec_s1  = Wire(Vec(8,  UInt(8.W)))
  val swapDataVec_s1  = Wire(Vec(16, UInt(8.W)))
  val compDataVec_s1  = Wire(Vec(16, UInt(8.W)))
  val initDataVec_s1  = Wire(Vec(16, UInt(8.W)))
  // S2: Use In Execute
  val valid_s2_g      = RegNext(valid_s1_g)
  val dealData_s2_g   = RegInit(0.U(128.W))
  // S2: Use In Out
  val firstIdx_s2_g   = RegInit(31.U)
  val lastIdx_s2_g    = RegInit(31.U)
  val inDataVec_s2_g  = RegEnable(inDataVec_s1, valid_s1_g)
  val outDataVec_s2   = Wire(Vec(32, UInt(8.W)))
  val dbID_s2_g       = RegEnable(in_s1_g.dbID, valid_s1_g)




// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------ S1: Parse Data Input ------------------------------------------------ //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Parse Data Input
   */
  val firstByte_s1    = PriorityEncoder(in_s1_g.atomic.mask)
  val bytesNum_s1     = PopCount(in_s1_g.atomic.mask).asUInt
  val halfBytesNum_s1 = (bytesNum_s1 >> 1).asUInt
  amoDataVec_s1       := in_s1_g.atomic.data.asTypeOf(amoDataVec_s1)
  inDataVec_s1        := in_s1_g.data.asTypeOf(inDataVec_s1)
  assert(bytesNum_s1 > 0.U | !valid_s1_g)

  /*
   * Parse loadData
   */
  loadDataVec_s1.zipWithIndex.foreach {
    case(load, i) =>
      load := Mux(i.U < bytesNum_s1, amoDataVec_s1(i.U + firstByte_s1), 0.U)
  }

  /*
   * Parse swapData
   */
  swapDataVec_s1.zipWithIndex.foreach {
    case(swap, i) =>
      when(op_s1 === SWAP) {
        swap := Mux(i.U < bytesNum_s1, amoDataVec_s1(i.U + firstByte_s1), 0.U)
      }.elsewhen(in_s1_g.atomic.swapFst) {
        swap := Mux(i.U < halfBytesNum_s1, amoDataVec_s1(i.U + firstByte_s1), 0.U)
      }.otherwise {
        swap := Mux(i.U < halfBytesNum_s1, amoDataVec_s1(i.U + firstByte_s1 + halfBytesNum_s1), 0.U)
      }
  }

  /*
   * Parse compData
   */
  compDataVec_s1.zipWithIndex.foreach {
    case (comp, i) =>
     when(in_s1_g.atomic.swapFst) {
        comp := Mux(i.U < halfBytesNum_s1, amoDataVec_s1(i.U + firstByte_s1 + halfBytesNum_s1), 0.U)
      }.otherwise {
        comp := Mux(i.U < halfBytesNum_s1, amoDataVec_s1(i.U + firstByte_s1), 0.U)
      }
  }

  /*
   * Parse initData
   */
  initDataVec_s1.zipWithIndex.foreach {
    case (init, i) =>
      when(op_s1 === COMPARE & in_s1_g.atomic.swapFst) {
        init := Mux(i.U < halfBytesNum_s1, inDataVec_s1(i.U + firstByte_s1 + halfBytesNum_s1), 0.U)
      }.elsewhen(op_s1 === COMPARE) {
        init := Mux(i.U < halfBytesNum_s1, inDataVec_s1(i.U + firstByte_s1), 0.U)
      }.otherwise {
        init := Mux(i.U < bytesNum_s1,     inDataVec_s1(i.U + firstByte_s1), 0.U)
      }
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------- S2 Excute Atomic --------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Get the absolute value
   */
  val sIntBit_s1  = ((bytesNum_s1 << 3.U).asUInt - 1.U).asTypeOf(UInt(6.W))
  def getAbs_s1(in: UInt): UInt = {
    val outVec    = Wire(Vec(64, Bool()))
    when(in(sIntBit_s1)){
      val temp    = ~(in - 1.U)
      temp.asBools.zipWithIndex.foreach { case(t, i) => outVec(i) := Mux(i.U < sIntBit_s1, t, false.B) }
    }.otherwise {
      outVec      := in.asBools
    }
    outVec.asUInt
  }

  when(valid_s1_g) {
    /*
     * AtomicLoad
     */
    when(op_s1 < SWAP) {
      // Get txnData and initData
      val txnData       = loadDataVec_s1.asTypeOf(UInt(64.W))
      val initData      = initDataVec_s1.asTypeOf(txnData)
      // Get
      switch(op_s1) {
        // LDADD
        is(LDADD) {
          dealData_s2_g := txnData + initData
        }
        // LDCLR
        is(LDCLR) {
          dealData_s2_g := initData & (~txnData).asUInt
        }
        // LDEOR
        is(LDEOR) {
          dealData_s2_g := initData ^ txnData
        }
        // LDSET
        is(LDSET) {
          dealData_s2_g := initData | txnData
        }
        // LDSMAX
        is(LDSMAX) {
          val txnBigger = Wire(Bool())
          when(txnData(sIntBit_s1) === initData(sIntBit_s1)) {
            val txnAbs  = Mux(txnData(sIntBit_s1), getAbs_s1(txnData), txnData)
            val initAbs = Mux(txnData(sIntBit_s1), getAbs_s1(initData), initData)
            txnBigger   := txnAbs > initAbs
          }.otherwise {
            txnBigger   := !txnData(sIntBit_s1)
          }
          dealData_s2_g := Mux(txnBigger, txnData, initData)
        }
        // LDSMIN
        is(LDSMIN) {
          val iniBigger = Wire(Bool())
          when(txnData(sIntBit_s1) === initData(sIntBit_s1)) {
            val txnAbs  = Mux(txnData(sIntBit_s1), getAbs_s1(txnData), txnData)
            val initAbs = Mux(txnData(sIntBit_s1), getAbs_s1(initData), initData)
            iniBigger   := txnAbs < initAbs
          }.otherwise {
            iniBigger   := txnData(sIntBit_s1)
          }
          dealData_s2_g := Mux(iniBigger, txnData, initData)
        }
        // LDUMAX
        is(LDUMAX) {
          dealData_s2_g := Mux(txnData > initData, txnData, initData)
        }
        // LDUMIN
        is(LDUMIN) {
          dealData_s2_g := Mux(txnData < initData, txnData, initData)
        }
      }
    /*
     * AtomicSwap
     */
    }.elsewhen(op_s1 === SWAP) {
      val swapData      = swapDataVec_s1.asTypeOf(UInt(64.W))
      dealData_s2_g     := swapData
    /*
     * AtomicCompare
     */
    }.elsewhen(op_s1 === COMPARE) {
      val swapData      = swapDataVec_s1.asTypeOf(UInt(128.W))
      val compData      = compDataVec_s1.asTypeOf(UInt(128.W))
      val initData      = initDataVec_s1.asTypeOf(compData)
      dealData_s2_g     := Mux(compData === initData, swapData, compData)
    /*
     * Other
     */
    }.otherwise {
      assert(false.B | !valid_s1_g)
    }
  }

// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------------- Output Result ---------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  when(valid_s1_g) {
    when(op_s1 === COMPARE & in_s1_g.atomic.swapFst) {
      firstIdx_s2_g := firstByte_s1 + halfBytesNum_s1
      lastIdx_s2_g  := firstByte_s1 + bytesNum_s1
    }.elsewhen(op_s1 === COMPARE) {
      firstIdx_s2_g := firstByte_s1
      lastIdx_s2_g  := firstByte_s1 + halfBytesNum_s1
    }.otherwise {
      firstIdx_s2_g := firstByte_s1
      lastIdx_s2_g  := firstByte_s1 + bytesNum_s1
    }
  }


  val dealDataVec_s2  = dealData_s2_g.asTypeOf(Vec(16, UInt(8.W)))
  outDataVec_s2.zipWithIndex.foreach {
    case(out, i) =>
      out := Mux(firstIdx_s2_g <= i.U & i.U < lastIdx_s2_g, dealDataVec_s2(i.U - firstIdx_s2_g), inDataVec_s2_g(i))
  }

  io.out.valid      := valid_s2_g
  io.out.bits.dbID  := dbID_s2_g
  io.out.bits.data  := outDataVec_s2.asUInt
}
















