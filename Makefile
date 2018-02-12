.PHONY: hlint_install, hlint, hlint_apply_refact, hlint_refactor
.PHONY: stylish_haskell_install, stylish_haskell_check, clean

STACK_BUILD := stack build

build: clean
	@$(STACK_BUILD)

watch: build
	@$(STACK_BUILD) --test --coverage --haddock --file-watch

test: clean
	@stack test

clean:
	@stack clean && rm *.cabal

# The following tasks cribbed from: https://lwm.github.io/haskell-static/
hlint_install:
	@stack install hlint

hlint: hlint_install
	@hlint src/ test/

hlint_apply_refact: hlint_install
	@stack install apply-refact

HLINT=hlint --refactor --refactor-options -i {} \;
hlint_refactor: hlint_apply_refact
	@find src/ test/ -name "*.hs" -exec $(HLINT)

stylish_haskell_install:
	@stack install stylish-haskell

STYLISH=stylish-haskell -i {} \;
stylish_haskell_check: stylish_haskell_install
	@find src/ test/ -name "*.hs" -exec $(STYLISH) && git diff --exit-code
