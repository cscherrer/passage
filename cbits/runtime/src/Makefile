-include extra_settings

CC=gcc

CPPFLAGS  +=-DNDEBUG
CFLAGS    +=-Wall -O3 -fomit-frame-pointer -msse2 -fopenmp

TGT=sampler

HEADER_FILES=passage.h mt19937ar.h
SRC_FILES=$(wildcard *.c)
OBJ_FILES=$(patsubst %.c,%.o,$(SRC_FILES))

$(TGT): $(HEADER_FILES) $(OBJ_FILES)
	$(CC) $(OBJ_FILES) -o $(TGT) -lm -fopenmp

clean:
	-rm $(TGT) *.o



