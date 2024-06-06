package coincreekDCache

import chisel3._
import chisel3.util._

class cachePipe_mshrFile_IO extends Bundle(){
    // val mshr_add_valid = Bool()
    val mshr_add_tag   = UInt(addrWidth.W)
    val mshr_add_type  = UInt(1.W) // 1 for write && 0 for read
    val mshr_add_mask  = UInt(mshrEntryMaskWidth.W)
    val mshr_add_data  = UInt(mshrEntryDataWidth.W)
}

class mshr_IO extends Bundle(){
    val mshr_req = Input(Bool())
    val mshr_tag = Input(UInt(tagWidth.W))

    val mshr_match = Output(Bool())
    val mshr_full  = Output(Bool())
    
}

class MSHR(id: Int) extends Module(){
    val io = IO(new mshr_IO)

    val mode_idle :: mode_req_enqueue :: mode_resp_wait :: mode_partial_replay ::
            mode_replay :: mode_refill :: Nil = Enum(6)
    val state = RegInit(mode_dile)

    // info regs
    val currentPermission = RegInit(0.U(1.W)) // 1 for write, 0 for read
    val upgradeRequest    = RegInit(0.U(1.W))

    val mshr_type_list    = RegInit(VecInit(Seq.fill(mshrEntryDataNum)(0.U(1.W))))
    val mshr_tag_reg      = RegInit(0.U(addrWidth.W))
    
    val totalCounter      = RegInit(0.U((log2Up(mshrEntryDataNum)).W))
    val pReplayCounter    = RegInit(0.U((log2Up(mshrEntryDataNum)).W))
    
    // match & output
    val mshr_match = mshr_req && (mshr_tag_reg === io.mshr_tag)
    val mshr_full  = mshr_req && (totalCounter >= (mshrEntryDataNum-1).asUInt)
    io.mshr_match := mshr_match
    io.mshr_full  := mshr_full
    
    // info regs update
    currentPermission := Mux((state > mode_req_enqueue), 
        currentPermission, 
        mshr_type_list.orR
    )

    upgradeRequest := Mux((state <= mode_req_enqueue),
        0.U,
        Mux(currentPermission, 0.U, mshr_type_list.orR)
    )

    mshr_type_list(totalCounter) := 


    // FSM
    state := MuxLookup(state, state)(
        Seq(
            mode_idle -> Mux(io.mshr_match, mode_req_enqueue, state),
            mode_req_enqueue -> 
        )
    )


}

class mshrFile extends Module(){
    val io = IO(new Bundle(
        val pipeline_req = Flipped(Decoupled(new cachePipe_mshrFile_IO))
        )
    )

    val mshr_mem = Module( // To store mask & data in sram
        new simSRAM_noMask(
            len = mshrEntryNum * mshrEntryDataNum
            width = mshrEntryMaskWidth + mshrEntryDataWidth
        ))
    
    
}