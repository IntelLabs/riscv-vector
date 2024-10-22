package gpc.core

import chisel3._
import chisel3.util._
import chisel3.{withClock,withReset}
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property

import freechips.rocketchip.rocket.{ExpandedInstruction, Instructions}
import freechips.rocketchip.rocket.{RVCExpander, IMM_UJ}

class PreDecoder(implicit p: Parameters) extends CoreModule{

    val io = IO(new Bundle {
        val stage_valid = Input(Bool())
        val pc = Input(UInt(vaddrBitsExtended.W)) //
        val data = Input(UInt((fetchWidth*coreInstBytes*8).W))
        val btb_resp = Input(Valid(new BTBResp()))
        val prev_is_half = Input(Bool())
        val prev_half_inst = Input(UInt(16.W))
        val inst_exp = Output(Vec(fetchWidth, UInt(32.W)))
        val inst = Output(UInt())
        val inst_mask = Output(Vec(fetchWidth, Bool()))
        val raw_insts = Output(Vec(fetchWidth, UInt(32.W)))
        val inst_pcs = Output((Vec(fetchWidth, UInt(vaddrBitsExtended.W))))
        val rvc = Output(Vec(fetchWidth,Bool()))
        val edge_inst = Output(Bool()) // True if 1st instruction in this bundle is pc - 2
        val end_half = Valid(UInt(16.W))
        val flush = Output(Bool())
        val predict_npc = Output(UInt(vaddrBitsExtended.W))
        val correct_redirect = Output(Bool())
        val is_call = Output(Bool())
        val is_return = Output(Bool())
        val is_branch = Output(Bool())
        val is_jump = Output(Bool())
        val redirect_return = Output(Bool())
        val btb_update = Output(Bool())
        val redirect_bridx = Output(UInt(log2Ceil(fetchWidth).W))
        val npc_plus4_mask = Output(Vec(fetchWidth, Bool())) 
    })

    io.redirect_bridx := 0.U
    io.flush := false.B
    io.predict_npc := DontCare
    io.redirect_return := false.B
    io.btb_update := false.B  
    
    def instisRVC(inst: UInt) = (inst(1,0) =/= 3.U)
    def isJALR(exp_inst: UInt) = exp_inst(6,0) === Instructions.JALR.value.asUInt(6,0)
    def isJump(exp_inst: UInt) = exp_inst(6,0) === Instructions.JAL.value.asUInt(6,0)  //JAL
    def isCall(exp_inst: UInt) = (isJALR(exp_inst) || isJump(exp_inst)) && exp_inst(7)
    def isRet(exp_inst: UInt)  = isJALR(exp_inst) && !exp_inst(7) && BitPat("b00?01") === exp_inst(19,15)
    def isBr(exp_inst: UInt)  = exp_inst(6,0) === Instructions.BEQ.value.asUInt(6,0)
    def fetchAlign(addr: UInt) = ~(~addr | (fetchBytes-1).U) // fetch group align
    def fetchMask(addr: UInt) = {
        val idx = addr.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
        ((BigInt(1) << fetchWidth)-1).U << idx
    }
    // def instDecoder(x: UInt) = {
    // val res = Wire(new ExpandedInstruction)
    // res.bits := x
    // res.rd := x(11,7)
    // res.rs1 := x(19,15)
    // res.rs2 := x(24,20)
    // res.rs3 := x(31,27)
    // res
    // }
    def ExpandRVC(inst: UInt)(implicit p: Parameters): UInt = {
    val rvc_exp = Module(new RVCExpander)
        rvc_exp.io.in := inst
    Mux(rvc_exp.io.rvc, rvc_exp.io.out.bits, inst)
    }
    
    val call_mask  = Wire(Vec(fetchWidth, Bool()))
    val ret_mask   = Wire(Vec(fetchWidth, Bool()))
    val br_mask   = Wire(Vec(fetchWidth, Bool()))
    val jump_mask = Wire(Vec(fetchWidth, Bool()))
    val jump_target = WireInit(0.U(vaddrBitsExtended.W)) 
    val aligned_pc = fetchAlign(io.pc)
    val fetch_mask = fetchMask(io.pc)
    val redirect = WireDefault(false.B)
    
    io.correct_redirect := redirect

    var redir_found = false.B
    io.is_call := (call_mask.asUInt & io.inst_mask.asUInt) =/= 0.U
    io.is_return  := (ret_mask.asUInt & io.inst_mask.asUInt) =/= 0.U
    io.is_branch := (br_mask.asUInt & io.inst_mask.asUInt) =/= 0.U
    io.is_jump := (jump_mask.asUInt & io.inst_mask.asUInt) =/= 0.U

    val last_inst = io.raw_insts(fetchWidth-1)(15,0)
    if(decodeWidth == 2){
        io.end_half.valid := (!(io.inst_mask(fetchWidth-2) && !instisRVC(io.raw_insts(fetchWidth-2))) && !instisRVC(last_inst))
    }else if (decodeWidth == 1) {
        io.end_half.valid := io.inst_mask(fetchWidth-1) && !instisRVC(last_inst)
    }
    
    io.end_half.bits := last_inst

    for (i <- 0 until fetchWidth) {
    val valid = Wire(Bool())
    io.inst_mask(i) := io.stage_valid && fetch_mask(i) && valid && !redir_found  
    io.inst_pcs(i) := aligned_pc + (i << 1).U - ((io.edge_inst && (i == 0).B) << 1)
    io.rvc(i) := instisRVC(io.raw_insts(i)) && io.inst_mask(i)
    io.inst := io.inst_exp(i)
    // wrong path
    when (!valid && io.btb_resp.valid && io.btb_resp.bits.bridx === i.U) {
        io.flush := true.B 
    }
    
    /*  If BTB miss, 
    1. decoded instruction is a jump(JAL) instruction 
        (The jump address can be calculated)
    2. decoded instruction is a branch instruction and the BHT predicts taken
    3. decoded instruction is Return 
        If BTB hit, but not taken 
    1. decoded instruction is a jump(JAL) instruction 
    2. decoded instruction is Return
        If BTB hit and taken
    1. decoded instruction is a jump(JAL) instruction, but predicted jump address is different from decoded
    --> redirect 
    */
    
    when (io.inst_mask(i) && (isJump(io.inst_exp(i)))) {
        jump_target := (io.inst_pcs(i).asSInt + ImmGen(IMM_UJ,io.inst_exp(i))).asUInt
    }

    when (!io.btb_resp.valid && io.inst_mask(i) && (isJump(io.inst_exp(i)) || isBr(io.inst_exp(i)) && io.btb_resp.bits.bht.taken) ||
            !io.btb_resp.bits.bht.taken && io.btb_resp.valid && io.inst_mask(i) && (isJump(io.inst_exp(i))) ||
            io.btb_resp.valid && io.btb_resp.bits.bht.taken && io.inst_mask(i) && (isJump(io.inst_exp(i))) && (io.btb_resp.bits.target =/= jump_target)){
                io.predict_npc := (io.inst_pcs(i).asSInt + ImmGen(IMM_UJ,io.inst_exp(i))).asUInt
                redirect := true.B
                io.btb_update := true.B
        }

    when((!io.btb_resp.valid || !io.btb_resp.bits.bht.taken) && io.inst_mask(i) && isRet(io.inst_exp(i))){
        redirect := true.B
        io.redirect_return := true.B
        io.btb_update := true.B
        io.redirect_bridx := i.U
    }
    
    call_mask(i) := isCall(io.inst_exp(i))
    ret_mask(i) := isRet(io.inst_exp(i))
    br_mask(i) := isBr(io.inst_exp(i))
    jump_mask(i) := isJump(io.inst_exp(i))
    io.npc_plus4_mask(i) := !instisRVC(io.raw_insts(i))
    if (i == 0)
    io.npc_plus4_mask(i) := !instisRVC(io.raw_insts(i)) && !io.edge_inst
    val redir_br = (isBr(io.inst_exp(i)) &&
        ((io.btb_resp.valid && io.btb_resp.bits.bridx === i.U && io.btb_resp.bits.taken && io.btb_resp.bits.bht.taken)))
    val redir = io.inst_mask(i) && (isJALR(io.inst_exp(i)) || isJump(io.inst_exp(i)) || redir_br)
    // Within the fetch group, if there are jump instructions or predicted branch jump instructions, 
    // subsequent instructions will be invalidated.
    redir_found = redir_found || redir  //TODO: need to change: add redirect condition
    if(decodeWidth == 2){
    if (i == 0) {
        valid := true.B
        when (io.prev_is_half) {
        val expanded = ExpandRVC(Cat(io.data(15,0), io.prev_half_inst))
        io.raw_insts(i) := Cat(io.data(15,0), io.prev_half_inst)
        io.inst_exp(i) := expanded
        io.edge_inst := true.B
        }.otherwise {
        val expanded = ExpandRVC(io.data(31,0))
        io.raw_insts(i) := io.data(31,0)
        io.inst_exp(i) := expanded
        io.edge_inst    := false.B
        }} else if (i == 1) {
      // Need special case since 0th instruction may carry over the wrap around
        val inst = io.data(47,16)
        val expanded = ExpandRVC(inst)
        io.raw_insts(i) := inst
        io.inst_exp(i) := expanded
        valid := io.prev_is_half || !(io.inst_mask(i-1) && !instisRVC(io.raw_insts(i-1)))
    } else if (i == fetchWidth - 1) {
        val inst = Cat(0.U(16.W), io.data(fetchWidth*16-1,(fetchWidth-1)*16))
        val expanded = ExpandRVC(inst)
        io.raw_insts(i) := inst
        io.inst_exp(i) := expanded
        valid := !((io.inst_mask(i-1) && !instisRVC(io.raw_insts(i-1))) || !instisRVC(inst))
    } else {
        val inst = io.data(i*16+32-1,i*16)
        val expanded = ExpandRVC(inst)
        io.raw_insts(i) := inst
        io.inst_exp(i) := expanded
        valid := !(io.inst_mask(i-1) && !instisRVC(io.raw_insts(i-1)))
    }
    }
}
}