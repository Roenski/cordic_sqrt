package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._
import scala.math.{pow, sqrt}

object SqrtDatatype extends ChiselEnum {
  val FLOAT, DOUBLE = Value
}

object FaultFlag extends ChiselEnum {
  val inexact   = Value(0.U)
  val underflow = Value(1.U)
  val overflow  = Value(2.U)
  val divByZero = Value(3.U)
  val invalid   = Value(4.U)
}

case class CORDICSqrtOutput() extends Bundle {
  val data   = UInt(64.W)
  val fflags = UInt(5.W)
}

case class FloatFields() extends Bundle {
  val mantissa = UInt(53.W)
  val exponent = UInt(11.W)
  val sign     = UInt(1.W)
  val mantRnd  = UInt(1.W) // Stores mantissa(0) if it was shifted right
}

trait CORDICMethods {

  def calcInverseCORDICGain(iterations: Int): Double = {
    var An = 1.0.toDouble
    var k  = 4
    for (i <- 1 to iterations) {
      An = An * sqrt(1 - pow(2, -2 * i))
      if (i == k) {
        An *= sqrt(1 - pow(2, -2 * i))
        k = 3 * k + 1
      }
    }
    print(1 / An)
    1 / An
  }

  val cordicInitialValue: Double = 0.25

  def generateRepeatIndices(iterations: Int): Seq[UInt] = {
    var kList   = Seq[UInt](4.U)
    var kTarget = 13
    for (i <- 1 to iterations) {
      if (i == kTarget) {
        kList = kList :+ i.U
        kTarget = 3 * i + 1
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

/** Compute floating-point square root with CORDIC algorithm. Dynamically selectable
  * between float and double
  */
class CORDICSqrtTop extends Module with CORDICMethods {
  import CORDICSqrtTop.State

  val io = IO(new Bundle {
    val in       = Flipped(ValidIO(UInt(64.W)))
    val datatype = Input(SqrtDatatype())
    val out      = ValidIO(CORDICSqrtOutput())
  })

  val iterations      = 100
  val calculationBits = 100

  // Submodules
  val preprocessor = Module(new PreProcessor)

  val cordicIter =
    Module(new CORDICSqrt(width = calculationBits, iterations = iterations))

  // Registers
  val in               = Reg(Output(chiselTypeOf(io.in)))
  val out              = Reg(Output(chiselTypeOf(io.out)))
  val cordicIn         = RegInit(0.U(54.W))
  val incrementCounter = WireDefault(false.B)
  val state            = RegInit(State.WAIT)
  val repeat           = RegInit(VecInit(generateRepeatIndices(iterations)))
  val repeatIndex      = RegInit(0.U(4.W))
  val xn               = RegInit(0.U)
  val yn               = RegInit(0.U)

  // Iterations counter
  val (iterCounterValue, iterCounterWrap) = Counter(incrementCounter, iterations + 1)

  val invCordicGain = WireDefault(((calcInverseCORDICGain(iterations) * scala.math.pow(
    2,
    calculationBits
  )).round).U)

  val cordicInit = WireDefault((cordicInitialValue / 2 * scala.math.pow(2, 54)).round.U)

  // Assignments
  io.out <> out
  cordicIter.in.iter := iterCounterValue

  // Initial values
  out.bits.data               := 0.U
  out.bits.fflags             := 0.U
  out.valid                   := false.B
  preprocessor.io.in.datatype := io.datatype
  preprocessor.io.in.data     := 0.U
  cordicIter.in.xn            := 0.U
  cordicIter.in.yn            := 0.U

  switch(state) {
    is(State.WAIT) {
      when(io.in.valid) {
        in <> io.in
        state := State.PREPROCESS
      }
    }
    is(State.PREPROCESS) {

      preprocessor.io.in.data := in.bits

      when(preprocessor.io.out.data.valid) {
        out <> preprocessor.io.out.data
        state := State.WAIT
      }.otherwise {
        cordicIn         := preprocessor.io.out.mantissa
        state            := State.CALCULATE
        incrementCounter := true.B
      }
    }
    is(State.CALCULATE) {

      when(iterCounterValue === 1.U) {
        // TODO: what if start value is 1.99? 1.99+0.25 will overflow
        cordicIter.in.xn := (cordicIn + cordicInit) << (calculationBits - 54)
        cordicIter.in.yn := (cordicIn - cordicInit) << (calculationBits - 54)
      }.otherwise {
        cordicIter.in.xn := xn
        cordicIter.in.yn := yn
        when(iterCounterValue === iterations.U) {
          state := State.FINISH
        }
      }

      when(iterCounterValue === repeat(repeatIndex)) {
        repeatIndex := repeatIndex + 1.U
      }.otherwise {
        incrementCounter := true.B
      }

      xn := cordicIter.out.xn1
      yn := cordicIter.out.yn1
    }
    is(State.FINISH) {
      val tempResult     = xn * invCordicGain
      val resultMantissa = tempResult(calculationBits, calculationBits - 52)
      out.bits.data    := tempResult(calculationBits, calculationBits - 64)
      out.bits.fflags  := 0.U // Hmm
      out.valid        := true.B
      state            := State.WAIT
      iterCounterValue := 1.U
    }
  }

}
