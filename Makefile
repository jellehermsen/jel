all: build
build:; cabal build
clean:; cabal clean
test:; cabal test > /dev/null; awk 'NR >= 3' dist/test/jel-*-test-jel.log
.PHONY: build clean test
