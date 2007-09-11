BASEDIR = ../..

SHARED_DIR=$(BASEDIR)/pir

include $(SHARED_DIR)/utils.make
include $(SHARED_DIR)/header.make

include $(BASEDIR)/rules.make


GHCFLAGS += -fallow-overlapping-instances
GHCFLAGS += -fglasgow-exts

GHCFLAGS += -i$(BASEDIR)/lib/haskell
GHCFLAGS += -i$(BASEDIR)/sfdl-compiler
GHCFLAGS += -i$(BASEDIR)/json/bnfc

GHCFLAGS += -v0


all: RandGraph


install: RandGraph
	$(INSTALL) $^ $(DIST_BIN)

RandGraph: RandGraph.hs
	ghc $(GHCFLAGS) --make -o $@ $^

clean:
	$(RM) RandGraph RandGraph.{o,hi}

dep:
