# Makefile for Gambit Scheme AoC day-9
SRC  := src/aoc_d9.scm
OUT  := build/d9

.PHONY: all clean

all: $(OUT)

$(OUT): $(SRC) | build
	gsc -verbose -exe -o $@ $<

build:
	mkdir -p $@

clean:
	rm -rf build
