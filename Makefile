PIRDIR=../../pir

include $(PIRDIR)/utils.make
include $(PIRDIR)/header.make

GHCFLAGS += -fallow-overlapping-instances
GHCFLAGS += -fglasgow-exts
GHCFLAGS += -i$(HOME)/work/code/lib/haskell

GHCFLAGS += -v0


all: RandGraph


install: RandGraph
	$(INSTALL) $^ $(LEEDS_BIN)

RandGraph: RandGraph.hs
	ghc $(GHCFLAGS) --make -o $@ $^
