
clean:
	cabal clean

build:
	cabal build --enable-tests --enable-benchmark --enable-documentation all

test:
	cabal run quasi-params-test

cachix:
	nix-store -qR --include-outputs `nix-instantiate nix/shell.nix` | cachix push maybevoid

.PHONY: clean build test
