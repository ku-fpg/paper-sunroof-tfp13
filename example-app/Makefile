
Main: *.hs
	ghc --make Main.hs -o Main -threaded \
             -fforce-recomp -dcore-lint -odir dist/ -hidir dist/

run: Main
	./Main

clean:
	rm -f Main
