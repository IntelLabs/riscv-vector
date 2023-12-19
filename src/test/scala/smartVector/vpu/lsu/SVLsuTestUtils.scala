package smartVector.lsutest

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import smartVector._
import SmartParam._

object test_init {
  def apply(dut: SmartVectorLsuTestWrapper): Unit = {
    dut.clock.setTimeout(0)
    dut.io.mUop.valid.poke(false.B)
    dut.io.mUop.bits.oldVd.poke(0.U)
  }
}
object next_is_load_and_step {
  def apply(dut: SmartVectorLsuTestWrapper): Unit = {
    dut.io.mUop.valid.poke(false.B)
    dut.io.mUop.bits.oldVd.poke(0.U)
    dut.clock.step(1)
  }
}

object test_init_store {
  def apply(dut: SmartVectorLsuStoreTestWrapper): Unit = {
    dut.clock.setTimeout(0)
    dut.io.mUop.valid.poke(false.B)
    // dut.io.mUop.bits.oldVd.poke(0.U)
  }
}

object next_is_store_and_step {
  def apply(dut: SmartVectorLsuStoreTestWrapper): Unit = {
    dut.io.mUop.valid.poke(false.B)
    // dut.io.mUop.bits.oldVd.poke(0.U)
    dut.clock.step(1)
  }
}

case class CtrlBundle(instrn: BitPat,
                      isLoad: Boolean = true,
                      vm: Boolean = true,
                      ma: Boolean = false,
                      ta: Boolean = true,
                      vsew: Int = 0,
                      vlmul: Int = 0,
                      vl: Int = 32,
                      vstart: Int = 0,
                      uopIdx: Int = 0,
                      uopEnd: Boolean = false,
                      vs2: Int = 0,
)

case class SrcBundleLd(scalar_opnd_1: String = "h1000",
                       scalar_opnd_2: String = "h0",
                       vs1: String = "hc0bfbebdbcbbbab9_b8b7b6b5b4b3b2b1",
                       vs2: String = "h0000000000000000_0000000000000000",
                       oldVd: String = "h201f1e1d1c1b1a19_1817161514131211",
                       mask: String = "h0",
                      //  mask: String = "hffff_ffff_ffff_ffff",
)

case class SrcBundleSt(scalar_opnd_1: String = "h1000",
                       scalar_opnd_2: String = "h0",
                       vs1: String = "hc0bfbebdbcbbbab9_b8b7b6b5b4b3b2b1",
                       vs2: String = "h0000000000000000_0000000000000000",
                       vs3: String = "h201f1e1d1c1b1a19_1817161514131211",
                       mask: String = "h0",
                      //  mask: String = "hffff_ffff_ffff_ffff",
)

trait BundleGenHelper {
  def genUop(c: CtrlBundle) = {
    (new VUopTest).Lit(
      _.ctrl_funct6 -> {if (c.instrn(31, 29).equals(BitPat("b???"))) {
                          BitPat.bitPatToUInt(c.instrn(28, 26))
                        } else {
                          BitPat.bitPatToUInt(c.instrn(31, 26))
                        }},
      _.ctrl_funct3 -> BitPat.bitPatToUInt(c.instrn(14, 12)),
      _.ctrl_load -> c.isLoad.B,
      _.ctrl_store -> (!c.isLoad).B,
      _.ctrl_vm -> c.vm.B,
      _.info_ma -> c.ma.B,
      _.info_ta -> c.ta.B,
      _.info_vsew -> c.vsew.U,
      _.info_vlmul -> c.vlmul.U,
      _.info_vl -> c.vl.U,
      _.info_vstart -> c.vstart.U,
      _.splitUopIdx -> c.uopIdx.U,
      _.splitUopEnd -> c.uopEnd.B,
      _.ctrl_vs2 -> {
        if(c.instrn(24, 20).equals(BitPat("b?????"))) {
          0.U
        } else {
          BitPat.bitPatToUInt(c.instrn(24, 20))
        }
      }
    )
  }

  def genLdInput(c: CtrlBundle, s: SrcBundleLd) = {
    (new MuopTest).Lit(
        _.uop -> genUop(c),
        _.scalar_opnd_1 -> s.scalar_opnd_1.U,
        _.scalar_opnd_2 -> s.scalar_opnd_2.U,
        _.vs1 -> s.vs1.U,
        _.vs2 -> s.vs2.U,
        _.vs3 -> 0.U,
        _.oldVd -> s.oldVd.U,
        _.mask -> s.mask.U,
    )
  }

  def genStInput(c: CtrlBundle, s: SrcBundleSt) = {
    (new MuopTest).Lit(
        _.uop -> genUop(c),
        _.scalar_opnd_1 -> s.scalar_opnd_1.U,
        _.scalar_opnd_2 -> s.scalar_opnd_2.U,
        _.vs1 -> s.vs1.U,
        _.vs2 -> s.vs2.U,
        _.vs3 -> s.vs3.U,
        _.oldVd -> 0.U,
        _.mask -> s.mask.U,
    )
  }
}


class VUopTest extends Bundle {
    val ctrl_vs2    = UInt(5.W)
    val ctrl_funct6 = UInt(6.W)
    val ctrl_funct3 = UInt(3.W)
    val ctrl_load   = Bool()
    val ctrl_store  = Bool()
    val ctrl_vm     = Bool()
    val info_ma     = Bool() // vector mask agnostic, data unknown or undisturbed
    val info_ta     = Bool() // vector tail agnostic, data unknown or undisturbed
    val info_vsew   = UInt(3.W)
    val info_vlmul  = UInt(3.W)
    val info_vl     = UInt(bVL.W)
    val info_vstart = UInt(bVstart.W)
    val splitUopIdx = UInt(3.W)
    val splitUopEnd = Bool()
}

class MuopTest extends Bundle {
    val uop           = new VUopTest
    val oldVd         = UInt(VLEN.W)
    val scalar_opnd_1 = UInt(XLEN.W)
    val scalar_opnd_2 = UInt(XLEN.W)
    val vs1           = UInt(VLEN.W)
    val vs2           = UInt(VLEN.W)
    val vs3           = UInt(VLEN.W)
    val mask          = UInt(VLEN.W)
}

class FakeLdDCache extends Module {
    val io = IO(Flipped(new RVUMemory))

    val dataTable = Seq(
        // addr, data, exception
        (0x0fd0.U, BigInt("4040404040404404", 16).U, false.B),
        (0x0fd8.U, BigInt("3030303030303030", 16).U, false.B),
        (0x0fe0.U, BigInt("2020202020202020", 16).U, false.B),
        (0x0fe8.U, BigInt("1010101010101010", 16).U, false.B),
        (0x0ff0.U, BigInt("5678901234503489", 16).U, false.B),
        (0x0ff8.U, BigInt("eeeeeeeeeeeeeeee", 16).U, false.B),
        (0x1000.U, BigInt("0123456789abcdef", 16).U, false.B),
        (0x1008.U, BigInt("ffffffffffffffff", 16).U, false.B),
        (0x1010.U, BigInt("0f0f0f0f0f0f0f0f", 16).U, false.B),
        (0x1018.U, BigInt("fedcba9876543210", 16).U, false.B),
        (0x1020.U, BigInt("1234567890123456", 16).U, false.B),
        (0x1028.U, BigInt("0101010101010101", 16).U, false.B),
        (0x1030.U, BigInt("2345678901234567", 16).U, false.B),
        (0x1038.U, BigInt("1111111111111111", 16).U, false.B),
        (0x1040.U, BigInt("2222222222222222", 16).U, false.B),
        (0x1048.U, BigInt("3333333333333333", 16).U, false.B),
        (0x1050.U, BigInt("4444444444444444", 16).U, false.B),
        (0x1058.U, BigInt("5555555555555555", 16).U, false.B),
        (0x1060.U, BigInt("6666666666666666", 16).U, true.B),
    )

    val hasXcpt = WireInit(false.B)
    val hasMiss = WireInit(false.B)

    val s1_valid = RegNext(io.req.valid)
    val s1_req   = RegNext(io.req.bits)
    val s2_valid = RegNext(s1_valid)
    val s2_req   = RegNext(s1_req)

    when(hasXcpt || hasMiss) {
        s1_valid := false.B
        s2_valid := false.B
    }

    val noise = RegInit("b011000001".U(32.W))
    noise := noise >> 1.U
    val miss = noise(0)

    io.resp.bits.idx        := s2_req.idx
    io.req.ready            := true.B
    io.xcpt                 := 0.U.asTypeOf(new HellaCacheExceptions())
    io.resp.bits.mask       := 0.U

    when(s2_valid) {
        val isXcpt = WireInit(false.B)
        val data = WireInit(0.U(64.W))
        for(i <- 0 until dataTable.length) {
            when(dataTable(i)._1 === s2_req.addr) {
              isXcpt := dataTable(i)._3
              data   := dataTable(i)._2
            }
        }

        io.resp.bits.nack     := miss
        io.resp.bits.data     := data
        io.resp.valid         := ~isXcpt && ~miss
        io.xcpt.ma.ld         := isXcpt && ~miss
        hasXcpt               := isXcpt && ~miss
        hasMiss               := miss

        io.resp.bits.has_data := ~isXcpt && ~miss
    }.otherwise {
        io.resp.valid         := false.B
        io.resp.bits.nack     := false.B
        io.resp.bits.has_data := false.B
        io.resp.bits.data     := 0.U
    }
}


class FakeStDCache extends Module {
    val io = IO(new Bundle {
      val dataExchange = Flipped(new RVUMemory)
      val memInfo = Output(Vec(20, UInt(64.W)))
    })

    val dataTable = Seq(
        // addr, exception
        (0x0fd0.U, false.B),
        (0x0fd8.U, false.B),
        (0x0fe0.U, false.B),
        (0x0fe8.U, false.B),
        (0x0ff0.U, false.B),
        (0x0ff8.U, false.B),
        (0x1000.U, false.B),
        (0x1008.U, false.B),
        (0x1010.U, false.B),
        (0x1018.U, false.B),
        (0x1020.U, false.B),
        (0x1028.U, false.B),
        (0x1030.U, false.B),
        (0x1038.U, false.B),
        (0x1040.U, false.B),
        (0x1048.U, false.B),
        (0x1050.U, false.B),
        (0x1058.U, false.B),
        (0x1060.U, true.B),
        (0x1068.U, false.B)
    )

    val dataVec = RegInit(VecInit(Seq.fill(dataTable.length)(VecInit(Seq.fill(8)(0.U(8.W))))))

    for (i <- 0 until dataTable.length) {
      val flattenedData = Cat(dataVec(i).reverse)
      io.memInfo(i) := flattenedData
    }
    
    io.dataExchange.req.ready            := true.B
    io.dataExchange.resp.bits.data       := 0.U
    io.dataExchange.resp.bits.nack       := false.B
    io.dataExchange.xcpt                 := 0.U.asTypeOf(new HellaCacheExceptions())
    io.dataExchange.resp.bits.has_data   := false.B

    val hasXcpt = WireInit(false.B)
    val hasMiss = WireInit(false.B)

    val s1_valid = RegNext(io.dataExchange.req.valid)
    val s1_req   = RegNext(io.dataExchange.req.bits)
    val s2_valid = RegNext(s1_valid)
    val s2_req   = RegNext(s1_req)

    when(hasXcpt || hasMiss) {
        s1_valid := false.B
        s2_valid := false.B
    }

    io.dataExchange.resp.valid           := s2_valid
    io.dataExchange.resp.bits.mask       := 0.U
    io.dataExchange.resp.bits.idx        := s2_req.idx

    val noise = RegInit("b011000001".U(32.W))
    noise := noise >> 1.U
    val random = noise(0)

    when(s2_valid) {
        when(random) {
            io.dataExchange.resp.bits.nack := true.B
            hasMiss := true.B
        }.otherwise {
            io.dataExchange.resp.bits.nack := false.B
            hasMiss := false.B
            for(i <- 0 until dataTable.length) {
              when(dataTable(i)._1 === s2_req.addr) {
                  when(dataTable(i)._2) {
                      io.dataExchange.xcpt.ma.ld := true.B
                      hasXcpt := true.B
                  }.otherwise {
                      io.dataExchange.xcpt.ma.ld := false.B
                      hasXcpt := false.B
                      when(s2_req.cmd === VMemCmd.write) {
                        for(j <- 0 until 8) {
                          when(s2_req.mask(j)) {
                              dataVec(i)(j) := s2_req.data(j * 8 + 7, j * 8)
                          }
                        }
                    }
                  }
              }
          }
        }
    }
}