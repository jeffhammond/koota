CC = mpicc
FC = gfortran

DEBUG = -g3 -ggdb
WARN = -Wall -Wextra -Werror -fmax-errors=3
CFLAGS = -std=c11 $(WARN) $(DEBUG)
FFLAGS = -std=f2018 -fall-intrinsics $(WARN) $(DEBUG)
LIBS = -L/opt/homebrew/Cellar/gcc/13.2.0/lib/gcc/13 -lgfortran

all: test.x

test.x: test.o koota.o verify.o
	$(CC) $(CFLAGS) $^ $(LIBS) -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $<

%.o: %.F90
	$(FC) $(FFLAGS) -c $<

clean:
	-rm -f test.x koota.o test.o *.mod

