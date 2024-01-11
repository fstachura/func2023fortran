main: src/*.hs
	ghc -W -dynamic -outputdir build -isrc src/main.hs -o main

clean:
	rm build/* main

