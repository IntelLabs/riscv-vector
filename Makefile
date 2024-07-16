
BUILD_DIR = ./build
CHISEL_VERSION = chisel3

PRJ = grapecoveDCache

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat cde

test:
	mill -i $(PRJ)[$(CHISEL_VERSION)].test

dcache:
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ)[$(CHISEL_VERSION)].runMain grapecoveDCache.Main --target-dir $(BUILD_DIR)

help:
	mill -i $(PRJ).runMain grapecoveDCache.DCache --help

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

clean:
	-rm -rf $(BUILD_DIR)
	rm -rf ./out
	rm -rf ./test_run_dir

include Makefile.test

.PHONY: test verilog help reformat checkformat clean