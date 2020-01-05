
clean:
	cabal v2-clean quiver-rows

build:
	cabal v2-build --enable-tests --enable-benchmark --enable-documentation all

repl:
	cabal v2-repl quiver-rows

doc:
	cabal v2-haddock quiver-rows

test:
	cabal v2-run quiver-rows-test

test-repl:
	cabal v2-repl quiver-rows-test

benchmark:
	cabal v2-run \
		quiver-rows-benchmark \
		-- --output '../benchmarks/$(shell date).html'

.PHONY: sync repl doc test test-repl benchmark
