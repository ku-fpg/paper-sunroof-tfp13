
Main:
	ghc --make Main.hs -o Main -threaded \
             -fforce-recomp -O2 -dcore-lint

run: Main
	./Main

clean:
	rm -f Main Main.o Main.hi main.js
