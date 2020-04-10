
clean:
	cabal clean

build:
	cabal build --enable-tests --enable-benchmark --enable-documentation all

test:
	cabal run dabble-test

test-nix:
	cabal --project-file=cabal-nix.project run dabble-test

cachix:
	nix-store -qR --include-outputs `nix-instantiate nix/shell.nix` | cachix push maybevoid

.PHONY: clean build test test-nix
