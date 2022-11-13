package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._
import scala.math._


/**
  * Compute floating-point square root with CORDIC algorithm.
  * Dynamically selectable between float and double
  */
class CORDICSqrt extends Module {
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val mantissa = UInt(53.W)
      val mant_rnd = UInt(1.W)
      val en       = Bool()
    })
    val out = new Bundle {
      val mantissa = ValidIO(UInt(53.W))
    }
  })

  val iterations = 60
  val inv_cordic_gain = calcInverseCORDICGain(iterations)

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
  
}