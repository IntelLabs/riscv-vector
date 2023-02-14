
import chisel3._
import chisel3.util._

package object yunsuan {
  def OpTypeWidth: Int = 6

  object OpType {
    def apply() = UInt(OpTypeWidth.W)
  }

  object VipuType {
    def dummy                          = "b001111".U(OpTypeWidth.W) // exu not implemented
    def add                            = "b000000".U(OpTypeWidth.W) // src1 + src2 //vadd vmadc
    def sub                            = "b000010".U(OpTypeWidth.W) // src1 - src2 //vsub vrsub vmsbc
    def addCarry                       = "b000001".U(OpTypeWidth.W) // src1 + src2 + carry //vadc vmadc
    def subBorrow                      = "b000011".U(OpTypeWidth.W) // src1 + borrow - src2 //vsbc vmsbc
    val unsignedWideningAdd            = "b000100".U(OpTypeWidth.W) //vwaddu
    val unsignedWideningsub            = "b000101".U(OpTypeWidth.W) //vwsubu
    val signedWideningAdd              = "b000110".U(OpTypeWidth.W) //vwadd
    val signedWideningSub              = "b000111".U(OpTypeWidth.W) //vwsub
    val unsignedWideningAddIn0Widening = "b001000".U(OpTypeWidth.W) //vwaddu
    val unsignedWideningSubIn0Widening = "b001001".U(OpTypeWidth.W) //vwsubu
    val signedWideningAddIn0Widening   = "b001010".U(OpTypeWidth.W) //vwadd
    val signedWideningSubIn0Widening   = "b001011".U(OpTypeWidth.W) //vwsub
    val maxUnsigned                    = "b001100".U(OpTypeWidth.W) //vmaxu
    val minUnsigned                    = "b001101".U(OpTypeWidth.W) //vminu
    val maxSigned                      = "b001110".U(OpTypeWidth.W) //vmax
    val minSigned                      = "b001111".U(OpTypeWidth.W) //vmin
    val equal                          = "b010000".U(OpTypeWidth.W) //vmseq
    val notEqual                       = "b010001".U(OpTypeWidth.W) //vmsne
    val lessThanUnsigned               = "b010010".U(OpTypeWidth.W) //vmsltu
    val lessThanSigned                 = "b010011".U(OpTypeWidth.W) //vmslt
    val lessThanOrEqualUnsigned        = "b010100".U(OpTypeWidth.W) //vmsleu
    val lessThanOrEqualSigned          = "b010101".U(OpTypeWidth.W) //vmsle
    val greaterThanUnsigned            = "b010110".U(OpTypeWidth.W) //vmsgtu
    val greaterThanSigned              = "b010111".U(OpTypeWidth.W) //vmsgt
    val greaterThanOrEqualUnsigned     = "b011000".U(OpTypeWidth.W) //vmsgeu
    val greaterThanOrEqualSigned       = "b011001".U(OpTypeWidth.W) //vmsge
    val vredsum                        = "b011100".U(OpTypeWidth.W) //vredsum_vs/vwredsum_vs/vwredsumu_vs
    val vredmax                        = "b011101".U(OpTypeWidth.W) //vredmax_vs/vredmaxu_vs  
    val vredmin                        = "b011110".U(OpTypeWidth.W) //vredmin_vs/vredminu_vs
    val vredand                        = "b011111".U(OpTypeWidth.W) // 
    val vredor                         = "b100000".U(OpTypeWidth.W) // 
    val vredxor                        = "b100001".U(OpTypeWidth.W) // 
    val vpopc                          = "b100010".U(OpTypeWidth.W) 
    val vfirst                         = "b100011".U(OpTypeWidth.W) 
    val vmsbf                          = "b100100".U(OpTypeWidth.W) 
    val vmsif                          = "b100101".U(OpTypeWidth.W) 
    val vmsof                          = "b100110".U(OpTypeWidth.W) 
    val viota                          = "b100111".U(OpTypeWidth.W) 
    val vid                            = "b101000".U(OpTypeWidth.W) 
    val vslideup                       = "b101001".U(OpTypeWidth.W) //vslideup_vx/vi        
    val vslidedn                       = "b101010".U(OpTypeWidth.W) //vslidedn_vx/vi        
    val vslide1up                      = "b101011".U(OpTypeWidth.W) //vslide1up_vx/vf       
    val vslide1dn                      = "b101100".U(OpTypeWidth.W) //vslide1dn_vx/vf       
    val vrgather                       = "b101101".U(OpTypeWidth.W) //vrgather_vv/vx/vi/vrgatherei16_vv        
    val vcompress                      = "b101110".U(OpTypeWidth.W) //vcompress_vm       
    // TODO: other op and method            
  }

  object VfpuType {
    def dummy         = "b11111111".U(OpTypeWidth.W) // exu not implemented
    def fadd          = "b10000000".U(OpTypeWidth.W) // src1 + src2
    def fmin          = "b10000001".U(OpTypeWidth.W) // fmin(src1,src2)
    def fmax          = "b10000010".U(OpTypeWidth.W) // fmax(src1,src2)
    def feq           = "b10000011".U(OpTypeWidth.W) // feq(src1,src2)
    def fne           = "b10000100".U(OpTypeWidth.W) // fne(src1,src2)
    def flt           = "b10000101".U(OpTypeWidth.W) // flt(src1,src2)
    def fle           = "b10000110".U(OpTypeWidth.W) // fle(src1,src2)
    def fgt           = "b10000111".U(OpTypeWidth.W) // fgt(src1,src2)
    def fge           = "b10001000".U(OpTypeWidth.W) // fge(src1,src2)
    def fsub          = "b10001001".U(OpTypeWidth.W) // src1 - src2
  }

  object VectorElementFormat {
    def width = 4
    def u8  = "b0000".U(width.W)
    def u16 = "b0001".U(width.W) 
    def u32 = "b0010".U(width.W) 
    def u64 = "b0011".U(width.W) 
    def s8  = "b0100".U(width.W)
    def s16 = "b0101".U(width.W) 
    def s32 = "b0110".U(width.W) 
    def s64 = "b0111".U(width.W) 
    def f16 = "b1001".U(width.W)
    def f32 = "b1010".U(width.W)
    def f64 = "b1011".U(width.W)
    def m1  = "b1111".U(width.W)

    def apply() = UInt(width.W)
  }


}
