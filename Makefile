# grab from the environment if needed.
DIST_ROOT ?= $(FAERIEPLAY_DIST_ROOT)

ifeq ($(DIST_ROOT),)
$(error Please set the environment variable FAERIEPLAY_DIST_ROOT)
endif

# where cabal puts the build
EXE = dist/build/randgraph/randgraph

all: $(EXE)

install: $(EXE)
	install $^ $(DIST_ROOT)/bin/

$(EXE): RandGraph.hs
	cabal configure && cabal build

clean:
	cabal clean
