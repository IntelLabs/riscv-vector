package darecreek.exu.fu.alu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import darecreek.ctrl.decode.VInstructions._
import VAluFunct6._

object VAluFunct6 {
  //                                  OPI (OPIVV/X/I)
  def VADD       = VADD_VV(31, 26)   ## BitPat("b1")
  def VSUB       = VSUB_VV(31, 26)   ## BitPat("b1")
  def VRSUB      = VRSUB_VX(31, 26)  ## BitPat("b1")
  def VWADDU     = VWADDU_VV(31, 26) ## BitPat("b0")
  def VWSUBU     = VWSUBU_VV(31, 26) ## BitPat("b0")
  def VWADD      = VWADD_VV(31, 26)  ## BitPat("b0")
  def VWSUB      = VWSUB_VV(31, 26)  ## BitPat("b0")
  def VWADDUWV   = VWADDU_WV(31, 26) ## BitPat("b0")
  def VWSUBUWV   = VWSUBU_WV(31, 26) ## BitPat("b0")
  def VWADDWV    = VWADD_WV(31, 26)  ## BitPat("b0")
  def VWSUBWV    = VWSUB_WV(31, 26)  ## BitPat("b0")
  def VEXT       = VZEXT_VF2(31, 26) ## BitPat("b0")
  def VADC       = VADC_VVM(31, 26)  ## BitPat("b1")
  def VMADC      = VMADC_VVM(31, 26) ## BitPat("b1")
  def VSBC       = VSBC_VVM(31, 26)  ## BitPat("b1")
  def VMSBC      = VMSBC_VVM(31, 26) ## BitPat("b1")
  def VAND       = VAND_VV(31, 26)   ## BitPat("b1")
  def VOR        = VOR_VV(31, 26)    ## BitPat("b1")
  def VXOR       = VXOR_VV(31, 26)   ## BitPat("b1")
  def VMSEQ      = VMSEQ_VV(31, 26)  ## BitPat("b1")
  def VMSNE      = VMSNE_VV(31, 26)  ## BitPat("b1")
  def VMSLTU     = VMSLTU_VV(31, 26) ## BitPat("b1")
  def VMSLT      = VMSLT_VV(31, 26)  ## BitPat("b1")
  def VMSLEU     = VMSLEU_VV(31, 26) ## BitPat("b1")
  def VMSLE      = VMSLE_VV(31, 26)  ## BitPat("b1")
  def VMSGTU     = VMSGTU_VX(31, 26) ## BitPat("b1")
  def VMSGT      = VMSGT_VX(31, 26)  ## BitPat("b1")
  def VMINU      = VMINU_VV(31, 26)  ## BitPat("b1")
  def VMIN       = VMIN_VV(31, 26)   ## BitPat("b1")
  def VMAXU      = VMAXU_VV(31, 26)  ## BitPat("b1")
  def VMAX       = VMAX_VV(31, 26)   ## BitPat("b1")
  // Fixed-Point
  def VSADDU     = VSADDU_VV(31, 26) ## BitPat("b1")
  def VSADD      = VSADD_VV(31, 26)  ## BitPat("b1")
  def VSSUBU     = VSSUBU_VV(31, 26) ## BitPat("b1")
  def VSSUB      = VSSUB_VV(31, 26)  ## BitPat("b1")
  def VAADDU     = VAADDU_VV(31, 26) ## BitPat("b0")
  def VAADD      = VAADD_VV(31, 26)  ## BitPat("b0")
  def VASUBU     = VASUBU_VV(31, 26) ## BitPat("b0")
  def VASUB      = VASUB_VV(31, 26)  ## BitPat("b0")
  def VSSRL      = VSSRL_VV(31, 26)  ## BitPat("b1")
  def VSSRA      = VSSRA_VV(31, 26)  ## BitPat("b1")
  def VNCLIPU    = VNCLIPU_WV(31, 26) ## BitPat("b1")
  def VNCLIP     = VNCLIP_WV(31, 26) ## BitPat("b1")
}

object VAluADD {
                      //          misc
                      //        sub |
                      //         |  |
  val default =         BitPat("b?  1")
  val table   = Seq(
    VADD             -> BitPat("b0  0"),
    VSUB             -> BitPat("b1  0"),
    VRSUB            -> BitPat("b0  0"),
    VWADDU           -> BitPat("b0  0"),
    VWSUBU           -> BitPat("b1  0"),
    VWADD            -> BitPat("b0  0"),
    VWSUB            -> BitPat("b1  0"),
    VWADDUWV         -> BitPat("b0  0"),
    VWSUBUWV         -> BitPat("b1  0"),
    VWADDWV          -> BitPat("b0  0"),
    VWSUBWV          -> BitPat("b1  0"),
    VADC             -> BitPat("b0  0"),
    VMADC            -> BitPat("b0  0"),
    VSBC             -> BitPat("b1  0"),
    VMSBC            -> BitPat("b1  0"),
    VMSEQ            -> BitPat("b?  0"),
    VMSNE            -> BitPat("b?  0"),
    VMSLTU           -> BitPat("b1  0"),
    VMSLT            -> BitPat("b1  0"),
    VMSLEU           -> BitPat("b1  0"),
    VMSLE            -> BitPat("b1  0"),
    VMSGTU           -> BitPat("b1  0"),
    VMSGT            -> BitPat("b1  0"),
    VMINU            -> BitPat("b1  0"),
    VMIN             -> BitPat("b1  0"),
    VMAXU            -> BitPat("b1  0"),
    VMAX             -> BitPat("b1  0"),
    // Fixed-Point
    VSADDU           -> BitPat("b0  0"),
    VSADD            -> BitPat("b0  0"),
    VSSUBU           -> BitPat("b1  0"),
    VSSUB            -> BitPat("b1  0"),
    VAADDU           -> BitPat("b0  0"),
    VAADD            -> BitPat("b0  0"),
    VASUBU           -> BitPat("b1  0"),
    VASUB            -> BitPat("b1  0"),
  )
}