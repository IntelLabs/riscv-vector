test_alu:
	sbt "testOnly darecreek.vfutest.alu.VAluSpec"

test_mac:
	sbt "testOnly darecreek.vfutest.alu.VMacSpec"

test_div:
	sbt "testOnly darecreek.vfutest.alu.VDivSpec"

test_load:
	sbt "testOnly darecreek.lsutest.VLsuSpec_ld"

# test_amd: test_alu, test_mac, test_div
test_amd: test_alu test_mac test_div

verilog:
	sbt "runMain darecreek.Main" -mem 4096

verilog_alu:
	sbt "runMain darecreek.exu.vfu.alu.Main"

verilog_mac:
	sbt "runMain darecreek.exu.vfu.mac.Main"

verilog_fp:
	sbt "runMain darecreek.exu.vfu.fp.Main"

verilog_div:
	sbt "runMain darecreek.exu.vfu.div.Main"

verilog_vmask:
	sbt "runMain darecreek.exu.vfu.vmask.Main"

verilog_reduction:
	sbt "runMain darecreek.exu.vfu.reduction.Main"

verilog_perm:
	sbt "runMain darecreek.exu.vfu.perm.Main"

verilog_allFu: verilog_alu verilog_mac verilog_fp verilog_div verilog_vmask verilog_reduction verilog_perm

clean:
	rm -rf ./build;	rm -rf ./generated;	rm -rf ./test_run_dir

.PHONY: verilog test_alu test_mac test_div test_amd clean
