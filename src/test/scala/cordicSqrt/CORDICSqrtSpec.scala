package cordicSqrt

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._

object floatValues {
  object SINGLE {
    val one = "h3f800000".U
    val two = "h40000000".U
    val four = "h40800000".U
    val three = "h40400000".U
    val nine = "h41100000".U
  }
  object DOUBLE {
    val one = "h3FF0000000000000".U
    val two = "h4000000000000000".U
    val three = "h4008000000000000".U
    val four = "h4010000000000000".U
    val nine = "h4022000000000000".U
  }
}

/**
  * This is a trivial example of how to run this Specification
  * From within sbt use:
  * {{{
  * testOnly cordiqSqrt.CORDICSqrtSpec
  * }}}
  * From a terminal shell use:
  * {{{
  * sbt 'testOnly cordiqSqrt.CORDICSqrtSpec'
  * }}}
  */
class CORDICSqrtSpec extends AnyFlatSpec with ChiselScalatestTester {

  def generateTestVector(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val data = source.getLines().map(_.split(" ")).toArray
    source.close()
    data
  }

  def testValue(dut: SqrtWrapper, input: UInt, expectedOutput: UInt) : Unit = {
    dut.io.in.bits  poke input
    dut.io.in.valid poke true.B
    dut.clock.step()
    dut.io.in.valid poke false.B
    while(dut.io.out.valid.peek().litToBoolean == false) dut.clock.step()
    val result = dut.io.out.bits.data.peek()
    val flags = dut.io.out.bits.fflags.peek()
    assert(result.litValue == expectedOutput.litValue, 
      s"Got: 0x${result.litValue.toString(16)}, Expected: 0x${expectedOutput.litValue.toString(16)}")
  }

  it should "check special cases for double" in {
    test(new SqrtWrapper(SqrtDatatype.DOUBLE)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val minus_zero = "h8000000000000000".U
      dut.io.in.bits  poke minus_zero
      dut.io.in.valid poke true.B
      // dut.io.datatype poke SqrtDatatype.DOUBLE
      dut.clock.step()
      dut.io.in.valid poke false.B
      while(dut.io.out.valid.peek().litToBoolean == false) dut.clock.step()
      dut.io.out.bits.data   expect minus_zero
      dut.io.out.bits.fflags expect 0.U
      dut.io.out.valid       expect true.B

      val plus_inf = "h7ff0000000000000".U
      dut.io.in.bits  poke plus_inf
      dut.io.in.valid poke true.B
      dut.clock.step()
      dut.io.in.valid poke false.B
      while(dut.io.out.valid.peek().litToBoolean == false) dut.clock.step()
      dut.io.out.bits.data   expect plus_inf
      dut.io.out.bits.fflags expect 0.U
      dut.io.out.valid       expect true.B

      val minus_inf  = "hfff0000000000000".U
      val minus_qNan = "hfff8000000000000".U
      dut.io.in.bits  poke minus_inf
      dut.io.in.valid poke true.B
      dut.clock.step()
      dut.io.in.valid poke false.B
      while(dut.io.out.valid.peek().litToBoolean == false) dut.clock.step()

      dut.io.out.bits.data   expect minus_qNan
      dut.io.out.bits.fflags expect 16.U
      dut.io.out.valid       expect true.B

      val qNan_with_payload = "h7ff8000000fafafa".U
      dut.io.in.bits  poke qNan_with_payload
      dut.io.in.valid poke true.B
      dut.clock.step()
      dut.io.in.valid poke false.B
      while(dut.io.out.valid.peek().litToBoolean == false) dut.clock.step()

      dut.io.out.bits.data   expect qNan_with_payload
      dut.io.out.bits.fflags expect 0.U
      dut.io.out.valid       expect true.B
    }
  }

  it should "check special cases for single" in {
    test(new SqrtWrapper(SqrtDatatype.SINGLE)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val minus_zero = "h80000000".U
      dut.io.in.bits  poke minus_zero
      dut.io.in.valid poke true.B
      // dut.io.datatype poke SqrtDatatype.DOUBLE
      dut.clock.step()
      dut.io.in.valid poke false.B
      while(dut.io.out.valid.peek().litToBoolean == false) dut.clock.step()
      dut.io.out.bits.data   expect minus_zero
      dut.io.out.bits.fflags expect 0.U
      dut.io.out.valid       expect true.B

      val plus_inf = "h7f800000".U
      dut.io.in.bits  poke plus_inf
      dut.io.in.valid poke true.B
      dut.clock.step()
      dut.io.in.valid poke false.B
      while(dut.io.out.valid.peek().litToBoolean == false) dut.clock.step()
      dut.io.out.bits.data   expect plus_inf
      dut.io.out.bits.fflags expect 0.U
      dut.io.out.valid       expect true.B

      val minus_inf  = "hff800000".U
      val minus_qNan = "hffc00000".U
      dut.io.in.bits  poke minus_inf
      dut.io.in.valid poke true.B
      dut.clock.step()
      dut.io.in.valid poke false.B
      while(dut.io.out.valid.peek().litToBoolean == false) dut.clock.step()

      dut.io.out.bits.data   expect minus_qNan
      dut.io.out.bits.fflags expect 16.U
      dut.io.out.valid       expect true.B

      val qNan_with_payload = "hffc0afaf".U
      dut.io.in.bits  poke qNan_with_payload
      dut.io.in.valid poke true.B
      dut.clock.step()
      dut.io.in.valid poke false.B
      while(dut.io.out.valid.peek().litToBoolean == false) dut.clock.step()

      dut.io.out.bits.data   expect qNan_with_payload
      dut.io.out.bits.fflags expect 0.U
      dut.io.out.valid       expect true.B
    }
  }

  it should "calculate easy values for single" in {
    test(new SqrtWrapper(SqrtDatatype.SINGLE)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testValue(dut, floatValues.SINGLE.one, floatValues.SINGLE.one)
      testValue(dut, floatValues.SINGLE.four, floatValues.SINGLE.two)
      testValue(dut, floatValues.SINGLE.nine, floatValues.SINGLE.three)
    }
  }
  it should "calculate easy values for double" in {
    test(new SqrtWrapper(SqrtDatatype.DOUBLE)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testValue(dut, floatValues.DOUBLE.one, floatValues.DOUBLE.one)
      testValue(dut, floatValues.DOUBLE.four, floatValues.DOUBLE.two)
      testValue(dut, floatValues.DOUBLE.nine, floatValues.DOUBLE.three)
    }
  }

  // it should "calculate test vector for single" in {
  //   test(new SqrtWrapper(SqrtDatatype.SINGLE)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
  //     val testVecs = generateTestVector("src/test/scala/cordicSqrt/f32_sqrt_test_vectors.txt")
  //     for (vec <- testVecs) {
  //       val testInput = {"h" + vec(0)}.U
  //       val testOutput = {"h" + vec(1)}.U
  //       testValue(dut, testInput, testOutput)
  //       println(s"SUCCESS: $testInput -> $testOutput")
  //     }
  //   }
  // }
}
