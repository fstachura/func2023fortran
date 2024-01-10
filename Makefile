main: src/*.hs
	ghc -dynamic -outputdir build -isrc src/main.hs -o main

clean:
	rm build/* main

