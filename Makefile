TNAME = funnlang
ifeq ($(OS), Windows_NT)
	TNAME = funnlang.exe
endif

DNAME = target/release/funnlang
ifeq ($(OS), Windows_NT)
	DNAME = target/release/funnlang.exe
endif

build:
	-cargo clean -p funnlang --release
	cargo build -r
	-rm $(TNAME) -f
	-mv $(DNAME) .
