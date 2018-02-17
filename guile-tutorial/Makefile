CC=gcc
CFLAGS = `guile-config compile` -Wall -g
LIBS = `guile-config link` -lm

#_DEPS = hellomake.h
#DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

DEPS =
OBJ = tortoise.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

tortoise: $(OBJ)
	gcc -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f *.o *~ core
