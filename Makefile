CFLAGS	= -Wall -Wextra -pedantic -std=c99
LDFLAGS	= -L/usr/local/lib
LDLIBS	= -lm -lncurses

sources = src/main.c src/memory/mem.c src/cpu/cpu.c src/cpu/instruction.c src/interface/interface.c src/interface/kbinput.c
headers = src/memory/mem.h src/cpu/cpu.h src/cpu/instruction.h src/interface/interface.h src/interface/kbinput.h src/utils/misc.h
	
koke: $(sources) $(headers)
	@mkdir -p bin
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(sources) $(LDLIBS)

clean:
	rm -rf bin