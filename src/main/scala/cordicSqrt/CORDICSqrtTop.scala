package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._
import scala.math.{pow,sqrt}

object SqrtDatatype extends ChiselEnum {
  val FLOAT, DOUBLE = Value
}


object FaultFlag extends ChiselEnum {
  val inexact     = Value(0.U)
  val underflow   = Value(1.U)
  val overflow    = Value(2.U)
  val div_by_zero = Value(3.U)
  val invalid     = Value(4.U)
}

case class CORDICSqrtOutput() extends Bundle {
  val data   = UInt(64.W)
  val fflags = UInt(5.W)
}

case class FloatFields() extends Bundle {
  val mantissa = UInt(53.W)
  val exponent = UInt(11.W)
  val sign     = UInt(1.W)
  val mant_rnd = UInt(1.W) // Stores mantissa(0) if it was shifted right
}


trait CORDICMethods {
  def calcInverseCORDICGain(iterations: Int) : Double = {
    var An = 1.0.toDouble
    var k = 4
    for (i <- 1 to iterations) {
      An = An * sqrt(1 - pow(2,-2*i))
      if (i == k) {
        An *= sqrt(1 - pow(2,-2*i))
        k = 3*k + 1
      }
    }
    1/An
  }

  def calcInitialValue(): UInt = {
     Cat(0.U, 0.U, 1.U, Fill(50, 0.U))
  }

  def generateRepeatIndices(iterations: Int): Seq[UInt] = {
    var i = 0
    val kList = Seq[UInt]()
    var kTarget = 4
    for (i <- 1 to iterations) {
      if (i == kTarget) {
        kList :+ i.U
        kTarget = 3*i + 1
      }
    }
    kList
  }
}

object CORDICSqrtTop {
  object State extends ChiselEnum {
    val WAIT, PREPROCESS, CALCULATE, FINISH = Value
  }
}

/**
  * Compute floating-point square root with CORDIC algorithm.
  * Dynamically selectable between float and double
  */
class CORDICSqrtTop extends Module with CORDICMethods {
  import CORDICSqrtTop.State

  val io = IO(new Bundle {
    val in = Flipped(ValidIO(UInt(64.W)))
    val datatype = Input(SqrtDatatype())
    val out = ValidIO(CORDICSqrtOutput())
  })

  val iterations = 100

  // Submodules
  val preprocessor = Module(new PreProcessor)
  val cordicIter  = Module(new CORDICSqrt(width=100, iterations=iterations))

  // Registers
  val in_r         = Reg(Output(chiselTypeOf(io.in)))
  val out_r        = Reg(Output(chiselTypeOf(io.out)))
  val iter_cnt_r   = RegInit(1.U)
  val state_r      = RegInit(State.WAIT)
  val repeat_r     = RegInit(VecInit(generateRepeatIndices(iterations)))
  val repeat_idx_r = RegInit(0.U)

  val inv_cordic_gain = calcInverseCORDICGain(iterations)
  val cordic_init = calcInitialValue()

  // Assignments

  io.out        <> out_r
  cordicIter.in.iter := iter_cnt_r

  // Initial values
  out_r.bits.data             := 0.U
  out_r.bits.fflags           := 0.U
  out_r.valid                 := false.B
  preprocessor.io.in.datatype := io.datatype
  preprocessor.io.in.data     := 0.U

  switch (state_r) {
    is (State.WAIT) {
      when (io.in.valid) {
        in_r <> io.in
        state_r := State.PREPROCESS
      }
    }
    is (State.PREPROCESS) {

      preprocessor.io.in.data := in_r.bits

      when (preprocessor.io.out.data.valid) {
        out_r   <> preprocessor.io.out.data
        state_r := State.WAIT
      } .otherwise {
        state_r    := State.CALCULATE
        iter_cnt_r := 1.U
      }
    }
    is (State.CALCULATE) {
      val xn = 0.U
      val yn = 0.U
      when (iter_cnt_r === 1.U) {
        xn := in_r.bits + cordic_init
        yn := in_r.bits - cordic_init
      } .otherwise {
        xn := cordicIter.out.xn1
        yn := cordicIter.out.yn1
        when (iter_cnt_r === iterations.U) {
          state_r := State.FINISH
          iter_cnt_r := 1.U
        }
      }
      cordicIter.in.xn := xn
      cordicIter.in.yn := yn
      when (iter_cnt_r === repeat_r(repeat_idx_r)) {
        repeat_idx_r := repeat_idx_r + 1.U
      } .otherwise {
        iter_cnt_r := iter_cnt_r + 1.U
      }
    }
    is (State.FINISH) {
      out_r.bits.data   := cordicIter.out.xn1
      out_r.bits.fflags := 0.U // Hmm
      out_r.valid       := true.B
      state_r           := State.WAIT
      iter_cnt_r        := 1.U
    }
  }

  when (in_r.valid) {
    preprocessor.io.in.data := in_r.bits
    // If preprocessor found an easy solution
    when (preprocessor.io.out.data.valid) {
      out_r <> preprocessor.io.out
    }
  }

}
