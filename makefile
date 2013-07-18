run :
	ghc --make -o simple_parser simple_parser.hs && \
	./simple_parser "1337" && \
	./simple_parser "#b1010" && \
	./simple_parser "#o1337" && \
	./simple_parser "#d1337" && \
	./simple_parser "#x1337" && \
	./simple_parser "#xdeadbabe"
clean :
	rm -f simple_parser *.hi *.o
