LIBRARY := HOCHPotential
LIBVER := 1

SHELL = /bin/sh
CC := gcc-6.2
FC := gfortran-6.2

FFLAGS := -O3 -fpic
CFLAGS:=-std=c99 -Werror -pedantic -Wall -W -Wmissing-prototypes -Wstrict-prototypes -Wconversion -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wwrite-strings -Wnested-externs -fshort-enums -fno-common -g -O4 -DHAVE_INLINE -fpic
HFLAGS:=-Wall

LDFLAGS:= -L/home/vale/toor/lib -L/home/vale/toor/lib64 -lm -lgfortran

SOURCES := potentialFFI.c
HEADERS := potentialFFI.h

FSOURCES := jbowman/bonds.f  jbowman/getpot.f  jbowman/h2co-ccsdt.f  jbowman/h2co-mrci.f
FOBJECTS = $(FSOURCES:.f=.o)
OBJECTS = $(SOURCES:.c=.o)

SHLIB = lib$(LIBRARY).so
SHLIBV = $(SHLIB).$(LIBVER)
STLIB = lib$(LIBRARY).a

all: $(SOURCES) $(HEADERS) $(LIBRARY)

$(LIBRARY): $(FOBJECTS) $(OBJECTS)
	$(CC) $(LDFLAGS) -shared -Wl,-soname,$(SHLIBV) -o $(SHLIBV).0 $(FOBJECTS) $(OBJECTS)
	ar rs lib$(LIBRARY).a $(FOBJECTS) $(OBJECTS)
	ln -sfT $(SHLIBV).0 $(SHLIB)
	ln -sfT $(SHLIBV).0 $(SHLIBV)

%.o : %.c
	$(CC) -c $(CFLAGS) $< -o $@

%.o : %.f
	$(FC) -c $(FFLAGS) $< -o $@

clean:
	rm $(OBJECTS) $(FOBJECTS)  lib$(LIBRARY).* || true
