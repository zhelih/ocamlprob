.PHONY: build clean target

target: build

clean:
	dune clean

build:
	dune build @all
