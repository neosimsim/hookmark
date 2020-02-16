# hookmark
[![pipeline status](https://gitlab.com/neosimsim/hookmark/badges/master/pipeline.svg)](https://gitlab.com/neosimsim/hookmark/commits/master)

Browser independent bookmark manager.

See `hookmark --help` for usage.

## integer-gmp
If you want to build without integer-gmp you can run

	cabal configure --constraint 'cryptonite -integer-gmp' --constraint 'hashable -integer-gmp' --constraint 'scientific +integer-simple' --constraint 'integer-logarithms -integer-gmp'
