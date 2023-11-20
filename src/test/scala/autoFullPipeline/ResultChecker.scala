package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import scala.collection.mutable.Map

import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import darecreek.exu.vfu.perm._
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.reduction._
import scala.collection.mutable.Map
import chipsalliance.rocketchip.config.Parameters

class ResultChecker(val n_ops : Int, val expectvd : Array[String], 
        val dump : (String, String) => Unit = (a, b) => {}) {
    var goldenVxsat : Boolean = false
    var goldenFflags : Int = 0

    var resVxsat : Boolean = false
    var resFflags : Int = 0

    var testVxsatOrFflags : Boolean = false
    var isFPDIV : Boolean = false

    var checkedRes = 0
    var uopIdxBitPat = 0

    var acked = 0

    def setGoldenVxsat(goldenVxsat : Boolean) = { 
        this.goldenVxsat = goldenVxsat
        this.isFPDIV = false
        this.testVxsatOrFflags = true
    }

    def setGoldenFflags(goldenFflags : Int) = {
        this.goldenFflags = goldenFflags
        this.isFPDIV = true
        this.testVxsatOrFflags = true
    }

    def isCompleted() : Boolean = { 
        this.checkedRes == this.n_ops 
    }

    def areAllAcked() : Boolean = {
        this.acked == this.checkedRes
    }

    def ack() = {
        assert(this.acked < this.checkedRes, "Acking more than already checked result")
        this.acked += 1
    }

    def checkBitPatCompleted() : Boolean = {
        ~(uopIdxBitPat & (~(1 << n_ops))) == 0
    }

    def checkRes(dutVd : BigInt, uopIdx : Int, 
            dutVxsat : Boolean = false, dutFflags : Int = 0) : Boolean = {
        
        assert(this.checkedRes < this.n_ops, s"received redundant result ${uopIdx}!!")
        
        if ((uopIdxBitPat & (1 << uopIdx)) > 0) 
            assert(false, s"uop ${uopIdx} has been checked twice!")
        else uopIdxBitPat |= (1 << uopIdx)

        var res = this._checkRes(dutVd, uopIdx)

        this.resFflags |= dutFflags
        this.resVxsat = this.resVxsat || dutVxsat

        this.checkedRes += 1
        if (res) {
            if (this.isCompleted() && this.testVxsatOrFflags) {
                if (this.isFPDIV) {
                    res = res && (this.goldenFflags == this.resFflags)
                    if (this.goldenFflags != this.resFflags) {
                        dump(f"(fflags) h${this.resFflags}%016x", f"(fflags) h${this.goldenFflags}%016x")
                    }
                } else {
                    res = res && (this.goldenVxsat == this.resVxsat)
                    if (this.goldenVxsat != this.resVxsat) {
                        dump(f"(vxsat) ${this.resVxsat}", f"(vxsat) ${this.goldenVxsat}")
                    }
                }
            }
        }

        return res
    }

    def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {
        println("!!!!!! _checkRes not implemented")
        false
    }
}