MODULES=deck command state authors UI 
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=UI.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS = oUnit

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)  -pkgs oUnit,ANSITerminal

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) 

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN) -use-ocamlfind -pkg ANSITerminal

check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

zip:
	zip UNO.zip *.ml* *.json _tags Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adv.zip
