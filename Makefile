TNAME = funnlang
ifeq ($(OS), Windows_NT)
	TNAME = funnlang.exe
endif

DNAME = target/release/funnlang
ifeq ($(OS), Windows_NT)
	DNAME = target/release/funnlang.exe
endif

DDNAME = target/debug/funnlang
ifeq ($(OS), Windows_NT)
	DDNAME = target/debug/funnlang.exe
endif

THREADS = $(shell nproc)

build:
	cargo build -r -j$(THREADS)
	-rm $(TNAME) -f
	-cp $(DNAME) .

debug:
	cargo build -j$(THREADS)
	-rm $(TNAME) -f
	-cp $(DDNAME) .

install:
	cargo build -r
	cargo install --path="."
