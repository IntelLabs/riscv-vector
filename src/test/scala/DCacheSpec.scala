package coincreekDCache

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class DCacheSpec extends AnyFreeSpec {

  "DCache should return resp" in {
    simulate(new DCache()) {
      dut =>
        dut.clock.step(10)

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.source.poke(1.U)
        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.source.expect(1.U)

    }
  }

}
