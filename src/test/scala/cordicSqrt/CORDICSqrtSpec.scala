package cordicSqrt

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._

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

  it should "check special cases" in {
    test(new CORDICSqrtTop) { dut =>
      val minus_zero = "h8000000000000000".U
      dut.io.in.bits  poke minus_zero
      dut.io.in.valid poke true.B
      dut.io.datatype poke SqrtDatatype.DOUBLE

      dut.io.out.bits.data   expect minus_zero
      dut.io.out.bits.fflags expect 0.U
      dut.io.out.valid       expect true.B
      dut.clock.step(1)

      val plus_inf = "h7ff0000000000000".U
      dut.io.in.bits  poke plus_inf
      dut.io.in.valid poke true.B
      dut.io.datatype poke SqrtDatatype.DOUBLE

      dut.io.out.bits.data   expect plus_inf
      dut.io.out.bits.fflags expect 0.U
      dut.io.out.valid       expect true.B

      val minus_inf  = "hfff0000000000000".U
      val minus_qNan = "hfff8000000000000".U
      dut.io.in.bits  poke minus_inf
      dut.io.in.valid poke true.B
      dut.io.datatype poke SqrtDatatype.DOUBLE

      dut.io.out.bits.data   expect minus_qNan
      dut.io.out.bits.fflags expect 16.U
      dut.io.out.valid       expect true.B

      val qNan_with_payload = "h7ff8000000fafafa".U
      dut.io.in.bits  poke qNan_with_payload
      dut.io.in.valid poke true.B
      dut.io.datatype poke SqrtDatatype.DOUBLE

      dut.io.out.bits.data   expect qNan_with_payload
      dut.io.out.bits.fflags expect 0.U
      dut.io.out.valid       expect true.B
    }
  }
}