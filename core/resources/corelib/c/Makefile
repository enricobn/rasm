#SAVE-TEMPS = -save-temps
CC = cc
LFLAGS = -Wall $(DEBUG)
CFLAGS = -c -std=c17 -Wno-incompatible-pointer-types -Wno-int-conversion $(DEBUG) $(OPTIMIZE) $(SAVE-TEMPS)

all : $(MAIN)

$(MAIN) : $(MAIN).o $(OTHERS)
	@echo 'Building $@'
	$(CC) $(LFLAGS) -o $@ $^ $(LIB)

%.o : %.c 
	@echo 'Compiling $<'
	$(CC) $(CFLAGS) $(DEBUG) $(INCLUDE) $<

.PHONY : clean

clean :
	rm -f $(MAIN); rm -f *.o; rm -f *.i; rm -f *.s; rm -f *.out