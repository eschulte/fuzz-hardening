CC=gcc

all: bin/limit bin/fuzz

bin/limit: bin/limit.c
	$(CC) $< -o $@

bin/fuzz: fuzz-2001/fuzz.c
	$(MAKE) -C fuzz-2001 fuzz; \
	cp fuzz-2001/fuzz $@

fuzz-2001/fuzz.c:
	mkdir -p fuzz-2001;
	cd fuzz-2001; \
	wget http://ftp.cs.wisc.edu/paradyn/fuzz/fuzz-2001/{fuzz.c,Makefile}
