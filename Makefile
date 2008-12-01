export PATH:=.:$(shell pwd)/extern/bin:$(PATH)
OCAMLC=ocamlc 
OCAMLOPT=ocamlopt 
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLDEP=ocamldep
OCAMLFIND=ocamlfind
OCAMLFLAGS=-cc g++
INCLUDES=-package extlib,unix 
LINKFLAGS=-linkpkg
OCAMLOPTFLAGS=

SRC_FILES := part3/son_of_blub.ml
ML_FILES  := $(filter %.ml,$(patsubst %.mll,%.ml,$(SRC_FILES:%.mly=%.ml)))
MLI_FILES := $(filter %.mli,$(SRC_FILES:%.mly=%.mli))
CMX_FILES := $(ML_FILES:%.ml=%.cmx) 
CMO_FILES := $(ML_FILES:%.ml=%.cmo)
CMI_FILES := $(MLI_FILES:%.mli=%.cmi) $(ML_FILES:%.ml=%.cmi)
GARBAGE := $(filter-out $(SRC_FILES),$(ML_FILES) $(MLI_FILES) $(CMX_FILES) $(CMX_FILES:%.cmx=%.o) $(CMO_FILES) $(CMI_FILES) interpreter .depend)

.SUFFIXES:

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) $(INCLUDES) $(OCAMLOPTFLAGS) -c $<

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) $(INCLUDES) $(OCAMLFLAGS) -c $<

%.cmx %.o: %.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) $(INCLUDES) $(OCAMLOPTFLAGS) -c $<

%.mli %.ml: %.mly
	$(OCAMLYACC) $<

%.ml: %.mll
	$(OCAMLLEX) $<

all : part1

LLVMMODULES:=llvm_bitwriter llvm_executionengine llvm llvm_scalar_opts
LLVMOC:=$(LLVMMODULES:%=%.cmxa)
LLVML:=$(addprefix -cclib ,$(LLVMMODULES:%=-l%))

part1 : $(CMX_FILES)
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) $(INCLUDES) $(LINKFLAGS) -o p2 $(LLVMOC) $(OCAMLOPTFLAGS) $^ $(LLVML)

clean ::
	rm -f $(GARBAGE)

.PRECIOUS: %/
extern/ extern/tar/ extern/build/ : %/ :
	mkdir -p $@
extern/tar/ extern/build/ : extern/

PREFIX:=$(shell pwd)/extern
EXTERNALS:=$(addprefix extern/bin/,ocamlc ocamlfind llvm-as) $(patsubst %,extern/lib/ocaml/site-lib/%/META,monad extlib)
WGET:=wget --progress=dot

extern/tar/llvm-2.3.tar.gz : | extern/tar
	$(WGET) http://llvm.org/releases/2.3/llvm-2.3.tar.gz -O $@
extern/build/llvm-2.3/configure: extern/tar/llvm-2.3.tar.gz | extern/build/
	tar -C extern/build -zxf $<
	touch $@
extern/bin/llvm-as: extern/build/llvm-2.3/configure | extern/bin/ocamlc extern/bin/ocamlfind
	cd $(<D) && ./$(<F) --prefix=$(PREFIX)
	$(MAKE) -C $(<D)
	$(MAKE) -C $(<D) install

extern/tar/ocaml-3.10.2.tar.gz : | extern/tar
	$(WGET) http://caml.inria.fr/pub/distrib/ocaml-3.10/ocaml-3.10.2.tar.gz -O $@
extern/build/ocaml-3.10.2/configure: extern/tar/ocaml-3.10.2.tar.gz | extern/build/
	tar -C extern/build -zxf $<
	touch $@
extern/bin/ocamlc: extern/build/ocaml-3.10.2/configure
	cd $(<D) && ./$(<F) -prefix $(PREFIX)
	$(MAKE) -C $(<D) world
	$(MAKE) -C $(<D) opt
	$(MAKE) -C $(<D) opt.opt
	$(MAKE) -C $(<D) install

extern/tar/findlib-1.2.1.tar.gz : | extern/tar
	$(WGET) http://download.camlcity.org/download/findlib-1.2.1.tar.gz -O $@
extern/build/findlib-1.2.1/configure: extern/tar/findlib-1.2.1.tar.gz | extern/build/
	tar -C extern/build -zxf $<
	touch $@
extern/bin/ocamlfind: extern/build/findlib-1.2.1/configure | extern/bin/ocamlc
	cd $(<D) && ./$(<F)
	$(MAKE) -C $(<D) all
	$(MAKE) -C $(<D) opt
	$(MAKE) -C $(<D) install

extern/tar/pa_monad.tar.gz : | extern/tar
	$(WGET) http://www.cas.mcmaster.ca/~carette/pa_monad/pa_monad.tar.gz -O $@
extern/build/pa_monad/META.in: extern/tar/pa_monad.tar.gz | extern/build/
	mkdir $(@D)
	tar -C $(@D) -zxf $<
	touch $@
extern/lib/ocaml/site-lib/monad/META : extern/build/pa_monad/META.in | extern/bin/ocamlc extern/bin/ocamlfind
	$(MAKE) -C $(<D)
	$(MAKE) -C $(<D) doc
	$(MAKE) -C $(<D) findlib-install

extern/tar/extlib-1.5.1.tar.gz : | extern/tar
	$(WGET) http://ocaml-extlib.googlecode.com/files/extlib-1.5.1.tar.gz -O $@
extern/build/extlib-1.5.1/Makefile : extern/tar/extlib-1.5.1.tar.gz | extern/build/
	tar -C extern/build -zxf $<
	touch $@
extern/lib/ocaml/site-lib/extlib/META : extern/build/extlib-1.5.1/Makefile | extern/bin/ocamlc extern/bin/ocamlfind
	$(MAKE) -C $(<D) all
	$(MAKE) -C $(<D) opt
	$(MAKE) -C $(<D) install

.depend: $(MLI_FILES) $(ML_FILES) | $(EXTERNALS) 
	$(OCAMLFIND) $(OCAMLDEP) $(INCLUDES) $^ > .depend

include .depend
