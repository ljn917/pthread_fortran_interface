LDFLAGS = $(LIB) -Wl,-rpath=$(OUTPUT_DIR)/lib
LDFLAGS_static = $(LDFLAGS) -static
STATIC_SUFFIX=_static
CSOURCES := $(wildcard *.c)
FSOURCES := $(wildcard *.f90)
CTARGETS := $(patsubst %.c,%,$(CSOURCES))
FTARGETS := $(patsubst %.f90,%,$(FSOURCES))
TEST_DIR := $(OUTPUT_DIR)/bin
INSTALL_TARGET := $(addprefix $(TEST_DIR)/, $(CTARGETS) $(FTARGETS) )


all: $(FTARGETS) $(CTARGETS)

%: %.c
	$(CC) -o $@ $< $(CFLAGS) $(CDEBUG) $(LDFLAGS) 
	$(CC) -o $@$(STATIC_SUFFIX) $< $(CFLAGS) $(CDEBUG) $(LDFLAGS_static) 
	#./$@

%: %.f90
	$(FC) -o $@ $< $(FFLAGS) $(FDEBUG) $(LDFLAGS) 
	$(FC) -o $@$(STATIC_SUFFIX) $< $(FFLAGS) $(FDEBUG) $(LDFLAGS_static) 
	#./$@

clean:
	rm -f $(CTARGETS) $(FTARGETS)
	rm -f $(addsuffix $(STATIC_SUFFIX), $(CTARGETS) $(FTARGETS) )

install: $(CTARGETS) $(FTARGETS) $(INSTALL_TARGET)
	cp -f run_all $(TEST_DIR)/run_all

$(TEST_DIR)/%: %
	cp -f $< $@
	cp -f $<$(STATIC_SUFFIX) $@$(STATIC_SUFFIX)

