package vpu.fp
import chiseltest._
import darecreek.exu.fp.VFDivSqrt
import org.scalatest.flatspec.AnyFlatSpec
import darecreek._

trait VFDivSqrtBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>
  val op_fdiv_32 = VCtrlBundle(
    funct6 = Integer.parseInt("100000", 2),
    isFp = true, typeTag = 0, isDivSqrt = true
  )
  val op_fdiv_64 = op_fdiv_32.copy(typeTag = 1)
  val op_frdiv_32 = op_fdiv_32.copy(
    funct6 = Integer.parseInt("100001", 2),
    divReverse = true
  )
  val op_frdiv_64 = op_frdiv_32.copy(typeTag = 1)
  val op_fsqrt_32 = op_fdiv_32.copy(
    funct6 = Integer.parseInt("010011", 2),
    isSqrt = true,
    lsrc0 = Integer.parseInt("00000", 2)
  )
  val op_fsqrt_64 = op_fsqrt_32.copy(typeTag = 1)
  def div_test(): Unit = {
    // vs2 / vs1 if not reverse.
    it should "vf(r)div" in {
      val ops = Seq(op_fdiv_32, op_fdiv_64, op_frdiv_32, op_frdiv_64)
      test(new VFDivSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val srcs = Seq(
            // 0.000001938479954333161, 991; -10.600000381469727, 0.800000011920929
            SrcBundle(vs1 = "h360216D54477C000", vs2 = "hC129999A3F4CCCCD"),
            SrcBundle(vs1 = "h402ACCCCCCCCCCCD", vs2 = "h41CEFE3C0D800000"),  // 1039955995 / 13.4
            SrcBundle(vs2 = "h360216D54477C000", vs1 = "hC129999A3F4CCCCD"),
            SrcBundle(vs2 = "h402ACCCCCCCCCCCD", vs1 = "h41CEFE3C0D800000"))  // 1039955995 / 13.4
          val ins = ops.zip(srcs).map(x => genFloatInput(x._2, x._1))
          dut.io.in.enqueueSeq(ins)
        }.fork {
          val fflags = Seq("h1", "h1", "h1", "h1")
          //  32:       1/-1.828754608086517821060759850879e-7, 1/1,238.7499815411867766202523252139
          //  64:       1/1.2885e-8
          val results = Seq(
            "hcaa6e0543a539eaa","h419280db415f85bb",
            "hcaa6e0543a539eaa","h419280db415f85bb"
          )
          val out = genFloatOutputs(ops, results, fflags)
          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
  }
  def sqrt_test(): Unit = {
    it should "vfsqrt" in {
      val ops = Seq(op_fsqrt_32, op_fsqrt_64)
      test(new VFDivSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          // operates on vs2
          val srcs = Seq(
            // -10.600000381469727, 0.800000011920929
            SrcBundle(vs1 = "h360216D54477C000", vs2 = "hC129999A3F4CCCCD"),
            // 1039955995
            SrcBundle(vs1 = "h402ACCCCCCCCCCCD", vs2 = "h41CEFE3C0D800000"))
          val ins = ops.zip(srcs).map(x => genFloatInput(x._2, x._1))
          dut.io.in.enqueueSeq(ins)
        }.fork {
          val fflags = Seq("h11", "h1")
          //  32:        NaN, 0.8944271802902222
          //  64:       32,248.34871741497402154918214198
          val results = Seq(
            "h7FC000003F64F92E","h40DF7E165162D937",
          )
          val out = genFloatOutputs(ops, results, fflags)
//          print(out(0).uop.typeTag, out(1).uop.typeTag)
//          dut.clock.step(80)
          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
  }

  def inactive_test(): Unit = {
    it should "skip inactive fflag" in {
      val op_sqrt_32_mask = op_fsqrt_32.copy(vm = false)
      val op_div_32_mask = op_fdiv_32.copy(vm = false)
      val ops = Seq(op_sqrt_32_mask, op_sqrt_32_mask, op_div_32_mask)
      test(new VFDivSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          // operates on vs2
          val srcs = Seq(
            // -10.600000381469727, 0.800000011920929
            SrcBundle(vs2 = "hC129999A3F4CCCCD", mask = "hff"),
            SrcBundle(vs2 = "hC129999A3F4CCCCD", mask = "hfd"),
            SrcBundle(vs1 = "h360216D54477C000", vs2 = "hC129999A3F4CCCCD", mask = "hfc"),
          )
          val ins = ops.zip(srcs).map(x => genFloatInput(x._2, x._1))
          dut.io.in.enqueueSeq(ins)
        }.fork {
          val fflags = Seq("h11", "h01", "h0")
          //  32:        NaN, 0.8944271802902222
          val results = Seq(
            "h7FC000003F64F92E", "h7FC000003F64F92E",
            "hcaa6e0543a539eaa"
          )
          val out = genFloatOutputs(ops, results, fflags)
          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
  }
}

class VFDivSqrtSpec extends AnyFlatSpec with ChiselScalatestTester
  with BundleGenHelper with VFDivSqrtBehavior {
  behavior of "VFDiv"
  it should behave like div_test
  it should behave like sqrt_test
  it should behave like inactive_test
}


