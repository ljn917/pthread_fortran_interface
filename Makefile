include $(var).mk

export

.PHONY: all src test clean install

all: src

src:
	$(MAKE) -C src

test: src
	$(MAKE) -C test

clean:
	$(MAKE) -C src $@
	$(MAKE) -C test clean

install:
	mkdir -p $(OUTPUT_DIR)/bin
	mkdir -p $(OUTPUT_DIR)/lib
	mkdir -p $(OUTPUT_DIR)/include
	$(MAKE) -C src install
	$(MAKE) -C test install
