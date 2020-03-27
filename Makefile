# Benchmarking
#
# Prepare by running "make mlkit_libs" in the MLKit repository directory after adding compilation steps for
# gengc...

MLKIT_ROOT?=`pwd`/../mlkit
MLKIT=$(MLKIT_ROOT)/bin/mlkit
KITBENCH=$(MLKIT_ROOT)/bin/kitbench

BENCHFILES=ray.mlb kbc.sml simple.sml mandelbrot.sml life.sml msort.sml msort-rf.sml mpuz.sml barnes-hut.mlb \
           logic.mlb DLX.sml professor.sml lexgen.sml tsp.sml vliw.sml mlyacc.mlb zebra.sml ratio.sml \
           fib37.sml tak.sml

#BENCHFILES=lexgen.sml

.PHONY: all
all: bench

.PHONY: bench
bench: bench.html

bench.html:
	MLKIT_ROOT=$(MLKIT_ROOT) $(KITBENCH) -o $@ -mlkit:-no_ri: -mlkit -mlkit:-gengc: -mlkit:-no_ri -gengc: -mlkit:-no_gc: $(BENCHFILES)

.PHONY: mlton
mlton: mlton.html

mlton.html:
	MLKIT_ROOT=$(MLKIT_ROOT) $(KITBENCH) -o $@ -mlton -mlkit:-no_gc: -mlkit $(BENCHFILES)

.PHONY: clean
clean:
	rm -rf MLB *~ *.exe *.stderr lllllll.txt run *.out.*.txt outFuhMishra* \
               */MLB */*.log *.html MLB *.auto.mlb lexgen *.frag */*~

%.gc.exe: %
	SML_LIB=$(MLKIT_ROOT) $(MLKIT) -o $*.gc.exe $<

%.gengc.exe: %
	SML_LIB=$(MLKIT_ROOT) $(MLKIT) -gengc -o $*.gengc.exe $<

%.norigc.exe: %
	SML_LIB=$(MLKIT_ROOT) $(MLKIT) -no_ri -o $*.norigc.exe $<

%.norigengc.exe: %
	SML_LIB=$(MLKIT_ROOT) $(MLKIT) -no_ri -gengc -o $*.norigengc.exe $<

%.frag: %.exe
	/bin/echo -n "$*:" > $@
	./$< -verbose_gc 2>&1 > /dev/null | grep -i Frag >> $@

FRAG_GC_FILES=$(BENCHFILES:%=%.gc.frag)
FRAG_GENGC_FILES=$(BENCHFILES:%=%.gengc.frag)
FRAG_NORIGC_FILES=$(BENCHFILES:%=%.norigc.frag)
FRAG_NORIGENGC_FILES=$(BENCHFILES:%=%.norigengc.frag)

frag.txt: $(FRAG_GC_FILES) $(FRAG_GENGC_FILES) $(FRAG_NORIGC_FILES) $(FRAG_NORIGENGC_FILES)
	cat $(FRAG_GC_FILES) $(FRAG_GENGC_FILES) $(FRAG_NORIGC_FILES) $(FRAG_NORIGENGC_FILES) > $@
	cat $@
