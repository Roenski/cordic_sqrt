package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._
import scala.math.{pow, sqrt}
import dataclass.data

object SqrtDatatype extends Enumeration{
  type SqrtDatatype = Value
  val SINGLE = Value(0)
  val DOUBLE = Value(1)
}
import SqrtDatatype._

object FaultFlag extends ChiselEnum {
  val inexact   = Value(0.U)
  val underflow = Value(1.U)
  val overflow  = Value(2.U)
  val divByZero = Value(3.U)
  val invalid   = Value(4.U)
}

case class CORDICSqrtOutput(datatype: SqrtDatatype)
    extends Bundle with datatypeMux {
  val mantissa  = UInt(datatypeMux(datatype, 24, 53).W)
  val exponent  = UInt(datatypeMux(datatype, 8, 11).W)
  val signBit   = UInt(1.W)
  val guardBit  = UInt(1.W)
  val roundBit  = UInt(1.W)
  val stickyBit = UInt(1.W)
  val fflags    = UInt(5.W)
  val special   = UInt(1.W)
}

case class FloatConsts(val datatype: SqrtDatatype)
    extends datatypeMux {
  // Doesn't include hidden bit
  val dataLength     = datatypeMux(datatype, 32, 64)
  val mantissaLength = datatypeMux(datatype, 23, 52)
  val exponentLength = datatypeMux(datatype, 8, 11)
  val bias           = datatypeMux(datatype, 127, 1023)
}

trait CORDICMethods {

    /**
    * Converts a Scala BigInt into Chisel UInt representation
    * Does not modify bits
    * For example, -1 BigInt would be turned into "hFFFFFFFF".U
    *
    * @param num BigInt input
    * @param XLEN Width of the data 
    * @return Chisel unsigned type
    */
  def BigIntToUInt(num: BigInt, XLEN: Int): UInt  = {
    if (num == 0) 0.U(XLEN.W)
    else if (num < 0) (num+pow(2,XLEN).toLong).U(XLEN.W)
    else num.U(XLEN.W)
  }

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
    println(1 / An)
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
    val WAIT, PREPROCESS, CALCULATE, MULTIPLY, FINISH = Value
  }

}

trait datatypeMux {

  def datatypeMux[T](datatype: SqrtDatatype, floatReturn: T, doubleReturn: T) = {
    if (datatype == SqrtDatatype.SINGLE) {
      floatReturn
    } else {
      doubleReturn
    }
  }

}

/** Compute floating-point square root with CORDIC algorithm. Dynamically selectable
  * between float and double
  */
class CORDICSqrtTop(val datatype: SqrtDatatype = SqrtDatatype.DOUBLE)
    extends Module with CORDICMethods with datatypeMux {
  import CORDICSqrtTop.State

  val consts = FloatConsts(datatype)

  val io = IO(new Bundle {
    val in       = Flipped(ValidIO(UInt(datatypeMux(datatype, 32, 64).W)))
    // val datatype = Input(SqrtDatatype())
    val out      = ValidIO(CORDICSqrtOutput(datatype))
  })

  val iterations      = datatypeMux(datatype, 50, 101)
  val calculationBits = datatypeMux(datatype, 50, 101)

  // Submodules
  val preprocessor = Module(new PreProcessor(datatype))
  val cordicIter =
    Module(new CORDICSqrt(width = calculationBits, iterations = iterations))

  // Registers
  val in          = Reg(Output(chiselTypeOf(io.in)))
  val out         = Reg(Output(chiselTypeOf(io.out)))
  val cordicIn    = RegInit(0.U((consts.mantissaLength + 2).W))
  val state       = RegInit(State.WAIT)
  val repeat      = RegInit(VecInit(generateRepeatIndices(iterations)))
  val repeatIndex = RegInit(0.U(4.W))
  val xn          = RegInit(0.S)
  val yn          = RegInit(0.S)
  val tempResult  = RegInit(0.U)
  val stickyBit   = RegInit(0.U(1.W))
  val roundBit    = RegInit(0.U(1.W))
  val guardBit    = RegInit(0.U(1.W))
  val exponent    = RegInit(0.U(consts.exponentLength.W))

  // Wires
  val incrementCounter = WireDefault(false.B)

  // Iterations counter
  val (iterCounterValue, iterCounterWrap) = Counter(incrementCounter, iterations + 1)

  val invCordicGain = WireDefault(BigIntToUInt(BigDecimal.valueOf(calcInverseCORDICGain(iterations) * scala.math.pow(
    2,
    calculationBits-2
  )).toBigInt, calculationBits))

  val cordicInit = WireDefault((cordicInitialValue / 2 * scala.math.pow(
    2,
    datatypeMux(datatype, 25, 54)
  )).round.U)

  // Assignments
  io.out <> out
  cordicIter.in.iter := iterCounterValue

  // Initial values
  out.bits.mantissa           := 0.U
  out.bits.signBit            := 0.U
  out.bits.guardBit           := 0.U
  out.bits.roundBit           := 0.U
  out.bits.stickyBit          := 0.U
  out.bits.exponent           := 0.U
  out.bits.fflags             := 0.U
  out.bits.special            := 0.U
  out.valid                   := false.B
  // preprocessor.io.in.datatype := 0.U
  preprocessor.io.in.data     := 0.U
  cordicIter.in.xn            := 0.S
  cordicIter.in.yn            := 0.S

  switch(state) {
    is(State.WAIT) {
      when(io.in.valid) {
        in <> io.in
        state := State.PREPROCESS
      }
    }
    is(State.PREPROCESS) {

      preprocessor.io.in.data := in.bits

      when(preprocessor.io.out.data.bits.special.asBool) {
        // Special case match
        out <> preprocessor.io.out.data
        state := State.WAIT
      }.otherwise {
        cordicIn         := preprocessor.io.out.mantissa
        exponent         := preprocessor.io.out.exponent
        state            := State.CALCULATE
      }
    }
    is(State.CALCULATE) {

      when(iterCounterValue === 0.U) {
        // TODO: what if start value is 1.99? 1.99+0.25 will overflow
        // TODO: share adder for initial value and cordic?
        val xnInit = Cat(0.U, cordicIn + cordicInit)
        xn := (xnInit << (calculationBits - datatypeMux(
          datatype,
          26,
          55
        ))).asSInt
        val ynInit = Cat(0.U, cordicIn - cordicInit)
        yn := (ynInit << (calculationBits - datatypeMux(
          datatype,
          26,
          55
        ))).asSInt
      }.otherwise {

        cordicIter.in.xn := xn
        cordicIter.in.yn := yn
        xn := cordicIter.out.xn1
        yn := cordicIter.out.yn1

        when(iterCounterValue === iterations.U) {
          state := State.MULTIPLY
        }
      }

      when(iterCounterValue === repeat(repeatIndex)) {
        repeatIndex := repeatIndex + 1.U
      }.otherwise {
        incrementCounter := true.B
      }

    }
    is (State.MULTIPLY) {
      val multiplyResult = (xn.asUInt.tail(1) * invCordicGain).tail(1)
      tempResult := multiplyResult.head(consts.mantissaLength + 1)
      guardBit   := multiplyResult.head(consts.mantissaLength + 3)(1)
      roundBit   := multiplyResult.head(consts.mantissaLength + 3)(0)
      stickyBit  := multiplyResult.tail(consts.mantissaLength + 3).orR
      state      := State.FINISH
    }
    is(State.FINISH) {
      out.bits.mantissa  := tempResult
      out.bits.exponent  := exponent
      out.bits.signBit   := 0.U
      out.bits.guardBit  := guardBit
      out.bits.roundBit  := roundBit
      out.bits.stickyBit := stickyBit
      out.bits.fflags    := 0.U // Hmm
      out.valid          := true.B
      state              := State.WAIT
      iterCounterValue   := 0.U
    }
  }

}
