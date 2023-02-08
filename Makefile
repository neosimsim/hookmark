check: phony
	cabal check
	cabal-fmt hookmark.cabal | diff hookmark.cabal -
	./misc/hlintCheck src test hookmark hookmark-web
	./misc/formatCheck src test hookmark hookmark-web

check-apply: phony
