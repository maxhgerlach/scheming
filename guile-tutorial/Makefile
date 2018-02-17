CC=gcc

#_DEPS = hellomake.h
#DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

DEPS =
LIBS = 
OBJ = tortoise.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

tortoise: $(OBJ)
	gcc -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f *.o *~ core
