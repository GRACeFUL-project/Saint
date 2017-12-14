
all:
	cd grammar && make && cd ..
	stack build
