build: clean
	rm -f dist/prack
	fpc -Fusrc -O3 -Os -CX -XX -Xs src/Prack.lpr
	mv src/Prack dist/prack

clean:
	rm -f src/*.o
	rm -f src/*.ppu
	rm -rf src/lib/
