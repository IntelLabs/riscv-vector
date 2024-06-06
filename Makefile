
BUILD_DIR = ./build

PRJ = coincreekDCache

test:
	mill -i $(PRJ).test

dcache:
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ).runMain coincreekDCache.Main --target-dir $(BUILD_DIR)

help:
	mill -i $(PRJ).runMain coincreekDCache.DCache --help

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: test verilog help reformat checkformat clean