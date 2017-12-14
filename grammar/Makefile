
all:
	bnfc -d Lambda.cf && cd Lambda && alex Lex.x && happy Par.y && cd ..
	-rm Lambda/*.bak
	-rm Lambda/Test.hs
	-rm Lambda/Doc.txt
