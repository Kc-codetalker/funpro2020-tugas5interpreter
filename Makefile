all:
	stack build
run:
	stack exec lambda-interpreter-exe
test:
	stack test
