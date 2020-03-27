mapo: Main.hs
	ghc -O $^ -o $@

.PHONY: clean
clean:
	rm -f *.o *.hi mapo
