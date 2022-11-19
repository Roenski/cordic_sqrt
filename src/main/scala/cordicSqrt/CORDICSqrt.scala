package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._
<<<<<<< HEAD

/**
  * One iteration of CORDIC for sqrt
  *
  */
class CORDICSqrt(width: Int, iterations: Int) extends Module {
  val in = IO(Input(new Bundle {
    val xn   = UInt(width.W)
    val yn   = UInt(width.W)
    val iter = UInt((math.log(iterations)/math.log(2)).toInt.W)
  }))
  val out = IO(Output(new Bundle {
    val xn1 = UInt(width.W)
    val yn1 = UInt(width.W)
  }))

  val xtemp = in.xn >> in.iter
  val ytemp = in.yn >> in.iter

  when (in.yn(width-1) === 1.U) {
    out.xn1 := in.xn + ytemp 
    out.yn1 := in.yn + xtemp
  } .otherwise {
    out.xn1 := in.xn - ytemp 
    out.yn1 := in.yn - xtemp
  }

=======
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
  
>>>>>>> 3e54dc882d8a9c88c8a4bae6a76ef572b76ed6fc
}