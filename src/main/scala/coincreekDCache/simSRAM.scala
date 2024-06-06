package coincreekDCache

import chisel3._
import chisel3.util._


class simSRAM_noMask(num: Int, width: Int) extends Module(){
    val io = IO(new Bundle{
        val r_ena  = Input(Bool())
        val r_addr = Input(UInt((log2Up(num)).W))
        val r_data = Output(UInt(width.W))

        val w_ena  = Input(Bool())
        val w_addr = Input(UInt((log2Up(num)).W))
        val w_data = Input(UInt(width.W))
    })

    val mem = SRAM(num, UInt(width.W), 1, 1, 0)
    
    mem.readPorts(0).enable := io.r_ena
    mem.readPorts(0).address := io.r_addr
    io.r_data := mem.readPorts(0).data

    mem.writePorts(0).enable := io.w_ena
    mem.writePorts(0).address := io.w_addr
    mem.writePorts(0).data := io.w_data
}

