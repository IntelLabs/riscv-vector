
BUILD_DIR = ./build
CHISEL_VERSION = chisel

PRJ = coincreekDCache

test:
	mill -i $(PRJ)[$(CHISEL_VERSION)].test

dcache:
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ)[$(CHISEL_VERSION)].runMain coincreekDCache.Main --target-dir $(BUILD_DIR)

meta:
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ)[$(CHISEL_VERSION)].runMain coincreekDCache.Meta --target-dir $(BUILD_DIR)

help:
	mill -i $(PRJ).runMain coincreekDCache.DCache --help

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

clean:
	-rm -rf $(BUILD_DIR)

include Makefile.test

.PHONY: test verilog help reformat checkformat clean