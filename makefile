run :
	ghc --make -o simple_parser simple_parser.hs && ./simple_parser
clean :
	rm -f simple_parser *.hi *.o
