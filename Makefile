all: build

build:
	@dune build

test:
	@dune runtest
