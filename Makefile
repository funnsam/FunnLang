FNAME = target/release/funnlang
ifeq ($(OS), Windows_NT)
	FNAME = target/release/funnlang.exe
endif

build:
	cargo clean -p funnlang --release
	cargo build -r
	mv $(FNAME) .