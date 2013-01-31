CC=gcc

bin/limit: bin/limit.c
	$(CC) $< -o $@

bin/fuzz: fuzz-2001/fuzz.c
	$(MAKE) -C fuzz-2001 fuzz; \
	cp fuzz-2001/fuzz $@
