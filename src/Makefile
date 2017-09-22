# see /usr/include/features.h
DEFINES = -D_XOPEN_SOURCE=700
LDFLAGS = -shared $(PTHREAD_FFLAG) $(LIB)

TARGET  = libpthread_f.so
TARGET_static  = libpthread_f.a
SOURCES := $(wildcard *.f90)
OBJECTS := $(patsubst %.f90,%.o,$(SOURCES))

LIB_INCLUDE_DIR := $(OUTPUT_DIR)/include
LIB_INSTALL_DIR := $(OUTPUT_DIR)/lib

all: $(TARGET) $(TARGET_static)

$(TARGET):  $(OBJECTS)
	$(FC) -o $@ $(DEBUG) $(LDFLAGS) $^

$(TARGET_static):  $(OBJECTS)
	rm -f $@
	$(AR) rvs $@ $^

%.o: %.c
	$(CC) -c -o $@ $< $(DEFINES) $(CFLAGS) $(CDEBUG)

%.o: %.f90 macro.h
	$(FC) -c -o $@ $< $(DEFINES) $(FFLAGS) $(FDEBUG)

%.o: %.mod
	true

macro.h: /usr/include/pthread.h
	$(CC) -dM -E $< -o $@

clean:
	rm -f *.o *.mod $(TARGET) $(TARGET_static)

install: $(TARGET_static) $(TARGET)
	cp -f $(TARGET) $(LIB_INSTALL_DIR)/$(TARGET)
	cp -f $(TARGET_static) $(LIB_INSTALL_DIR)/$(TARGET_static)
	cp -f *.mod $(LIB_INCLUDE_DIR)/

