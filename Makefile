all: build
build:; cabal build
clean:; cabal clean
test:; cabal test > /dev/null; awk 'NR >= 3' dist/test/jel-*-test-jel.log
docs:; cabal haddock --executables
.PHONY: build clean test
