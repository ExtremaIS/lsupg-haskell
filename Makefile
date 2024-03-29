##############################################################################
# Project configuration

PACKAGE     := lsupg
CABAL_FILE  := $(PACKAGE).cabal
PROJECT     := $(PACKAGE)-haskell
EXECUTABLES := lsupg

MODE ?= stack

CABAL_TEST_GHC_VERSIONS += 8.8.4
CABAL_TEST_GHC_VERSIONS += 8.10.7
CABAL_TEST_GHC_VERSIONS += 9.0.2
CABAL_TEST_GHC_VERSIONS += 9.2.2

STACK_TEST_CONFIGS += stack-8.8.4.yaml
STACK_TEST_CONFIGS += stack-8.10.7.yaml
STACK_TEST_CONFIGS += stack-9.0.2.yaml
STACK_TEST_CONFIGS += stack-9.2.2.yaml

CABAL_STATIC_REPO_8.8.4  := utdemir/ghc-musl:v24-ghc884
CABAL_STATIC_REPO_8.10.7 := utdemir/ghc-musl:v24-ghc8107
CABAL_STATIC_REPO_9.0.2  := utdemir/ghc-musl:v24-ghc902
CABAL_STATIC_REPO_9.2.2  := utdemir/ghc-musl:v24-ghc922

##############################################################################
# Make configuration

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error GNU Make 4.0 or later required)
endif
.RECIPEPREFIX := >

SHELL := bash
.SHELLFLAGS := -o nounset -o errexit -o pipefail -c

MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --warn-undefined-variables

.DEFAULT_GOAL := build

ifeq ($(MODE), cabal)
  GHC_VERSION ?= $(shell ghc --version | sed 's/.* //')
  CABAL_ARGS := --with-ghc ghc-$(GHC_VERSION)
  ifneq ($(origin PROJECT_FILE), undefined)
    CABAL_ARGS += "--project-file=$(PROJECT_FILE)"
  else
    PROJECT_FILE_AUTO := cabal-$(GHC_VERSION).project
    ifneq (,$(wildcard $(PROJECT_FILE_AUTO)))
      CABAL_ARGS += "--project-file=$(PROJECT_FILE_AUTO)"
    endif
  endif
  CABAL_STATIC_REPO :=
  ifneq ($(origin CABAL_STATIC_REPO_$(GHC_VERSION)), undefined)
    CABAL_STATIC_REPO := $(CABAL_STATIC_REPO_$(GHC_VERSION))
  endif
else ifeq ($(MODE), stack)
  STACK_ARGS :=
  ifneq ($(origin CONFIG), undefined)
    STACK_ARGS += --stack-yaml "$(CONFIG)"
  else
    CONFIG := stack.yaml
  endif
  ifneq ($(origin RESOLVER), undefined)
    STACK_ARGS += --resolver "$(RESOLVER)"
  endif
  ifneq ($(origin STACK_NIX_PATH), undefined)
    STACK_ARGS += "--nix-path=$(STACK_NIX_PATH)"
  endif
else
  $(error unknown MODE: $(MODE))
endif

##############################################################################
# Functions

define all_files
  find . -not -path '*/\.*' -type f
endef

define checksum_files
  find . -maxdepth 1 -type f -not -path './*SUMS' | sed 's,^\./,,' | sort
endef

define die
  (echo "error: $(1)" ; false)
endef

define get_version
$(shell grep '^version:' $(if $(origin 1) == undefined,$(CABAL_FILE),$(1)) \
        | sed 's/^version: *//')
endef

define hs_files
  find . -not -path '*/\.*' -type f -name '*.hs'
endef

define newline


endef

##############################################################################
# Rules

build: hr
build: # build package *
ifeq ($(MODE), cabal)
> cabal v2-build $(CABAL_ARGS)
else
> stack build $(STACK_ARGS)
endif
.PHONY: build

checksums: # calculate checksums of build artifacts
> @cd build && $(call checksum_files) | xargs md5sum > MD5SUMS
> @cd build && $(call checksum_files) | xargs sha1sum > SHA1SUMS
> @cd build && $(call checksum_files) | xargs sha256sum > SHA256SUMS
> @cd build && $(call checksum_files) | xargs sha512sum > SHA512SUMS
.PHONY: checksums

clean: # clean package
ifeq ($(MODE), cabal)
> @rm -rf dist-newstyle
else
> @stack clean
endif
.PHONY: clean

clean-all: clean
clean-all: # clean package and remove artifacts
> @rm -rf .hie
> @rm -rf .stack-work
> @rm -rf build
> @rm -rf dist-newstyle
> @rm -f *.yaml.lock
> @rm -f cabal.project.local
> @rm -f result*
.PHONY: clean-all

doc-api: hr
doc-api: # build API documentation *
ifeq ($(MODE), cabal)
> cabal v2-haddock $(CABAL_ARGS)
else
> stack haddock $(STACK_ARGS)
endif
.PHONY: doc-api

grep: # grep all non-hidden files for expression E
> $(eval E:= "")
> @test -n "$(E)" || $(call die,"usage: make grep E=expression")
> @$(call all_files) | xargs grep -Hn '$(E)' || true
.PHONY: grep

help: # show this help
> @grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
>   | sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
>   | column -t -s $$'\t'
> @echo
> @echo "Cabal mode (MODE=cabal)"
> @echo "  * Set GHC_VERSION to specify a GHC version."
> @echo "  * Set PROJECT_FILE to specify a cabal.project file."
> @echo
> @echo "Stack mode (MODE=stack)"
> @echo "  * Set CONFIG to specify a stack.yaml file."
> @echo "  * Set RESOLVER to specify a Stack resolver."
> @echo "  * Set STACK_NIX_PATH to specify a Stack Nix path."
.PHONY: help

hlint: # run hlint on all Haskell source
> @$(call hs_files) | xargs hlint
.PHONY: hlint

hr: #internal# display a horizontal rule
> @command -v hr >/dev/null 2>&1 && hr -t || true
.PHONY: hr

hsgrep: # grep all Haskell source for expression E
> $(eval E := "")
> @test -n "$(E)" || $(call die,"usage: make hsgrep E=expression")
> @$(call hs_files) | xargs grep -Hn '$(E)' || true
.PHONY: hsgrep

hsrecent: # show N most recently modified Haskell files
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -name '*.hs' -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: hsrecent

hssloc: # count lines of Haskell source
> @$(call hs_files) | xargs wc -l | tail -n 1 | sed 's/^ *\([0-9]*\).*$$/\1/'
.PHONY: hssloc

recent: # show N most recently modified files
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: recent

repl: # enter a REPL *
ifeq ($(MODE), cabal)
> cabal repl $(CABAL_ARGS)
else
> stack exec $(STACK_ARGS) ghci
endif
.PHONY: repl

source-git: # create source tarball of git TREE
> $(eval TREE := "HEAD")
> $(eval BRANCH := $(shell git rev-parse --abbrev-ref $(TREE)))
> @test "$(BRANCH)" = "main" || echo "WARNING: Not in main branch!" >&2
> $(eval DIRTY := $(shell git diff --shortstat | wc -l))
> @test "$(DIRTY)" = "0" \
>   || echo "WARNING: Not including non-committed changes!" >&2
> $(eval UNTRACKED := $(shell \
    git ls-files --other --directory --no-empty-directory --exclude-standard \
    | wc -l))
> @test "$(UNTRACKED)" = "0" \
>   || echo "WARNING: Not including untracked files!" >&2
> $(eval VERSION := $(call get_version))
> @mkdir -p build
> @git archive --format=tar --prefix=$(PROJECT)-$(VERSION)/ $(TREE) \
>   | xz \
>   > build/$(PROJECT)-$(VERSION).tar.xz
.PHONY: source-git

source-tar: # create source tarball using tar
> $(eval DIRTY := $(shell git diff --shortstat | wc -l))
> @test "$(DIRTY)" = "0" \
>   || echo "WARNING: Including non-committed changes!" >&2
> $(eval UNTRACKED := $(shell \
    git ls-files --other --directory --no-empty-directory --exclude-standard \
    | wc -l))
> @test "$(UNTRACKED)" = "0" \
>   || echo "WARNING: Including untracked files!" >&2
> $(eval VERSION := $(call get_version))
> @mkdir -p build
> @sed -e 's,^/,./,' -e 's,/$$,,' .gitignore > build/.gitignore
> @tar \
>   --exclude-vcs \
>   --exclude-ignore-recursive=build/.gitignore \
>   --transform "s,^\.,$(PROJECT)-$(VERSION)," \
>   -Jcf build/$(PROJECT)-$(VERSION).tar.xz \
>   .
> @rm -f build/.gitignore
.PHONY: source-tar

stan: hr
stan: export STAN_USE_DEFAULT_CONFIG=True
stan: # run stan static analysis
ifeq ($(MODE), cabal)
> @cabal v2-build -f write-hie
else
> @stack build --flag $(PACKAGE):write-hie
endif
> @stan
.PHONY: stan

static: hr
static: # build a static executable *
> $(eval VERSION := $(call get_version))
ifeq ($(MODE), cabal)
> @test -n "$(CABAL_STATIC_REPO)" \
>   || $(call die,"Docker repo not configured for GHC $(GHC_VERSION)")
> @rm -rf dist-newstyle
> @docker run --rm -it -v "$(CURDIR):/host" "$(CABAL_STATIC_REPO)" \
>   "/host/script/build-cabal-static.sh" --flags=static $(CABAL_ARGS)
> @mkdir -p "build"
> @cp "$$(find dist-newstyle -type f -name lsupg)" "build"
> @strip "build/lsupg"
> @cd build && tar -Jcvf "lsupg-$(VERSION).tar.xz" "lsupg"
else
> @test -f "$(CONFIG)" || $(call die,"$(CONFIG) not found")
> $(eval REPO := $(shell grep '^ *repo: "[^"]*"$$' "$(CONFIG)" \
    | sed -e 's/^[^"]*"//' -e 's/"$$//'))
> @test -n "$(REPO)" || $(call die,"Docker repo not configured in $(CONFIG)")
> @rm -rf .stack-work
> @stack build --stack-yaml $(CONFIG) --flag lsupg:static --docker
> @mkdir -p "build"
> @cp "$$(find .stack-work/install -type f -name lsupg)" "build"
> @cd build && tar -Jcvf "lsupg-$(VERSION).tar.xz" "lsupg"
endif
.PHONY: static

test: hr
test: # run tests, optionally for pattern P *
> $(eval P := "")
ifeq ($(MODE), cabal)
> @test -z "$(P)" \
>   && cabal v2-test $(CABAL_ARGS) --enable-tests --test-show-details=always \
>   || cabal v2-test $(CABAL_ARGS) --enable-tests --test-show-details=always \
>       --test-option '--pattern=$(P)'
else
> @test -z "$(P)" \
>   && stack test $(STACK_ARGS) \
>   || stack test $(STACK_ARGS) --test-arguments '--pattern $(P)'
endif
.PHONY: test

test-all: # run all configured tests
ifeq ($(MODE), cabal)
> $(foreach GHC_VERSION,$(CABAL_TEST_GHC_VERSIONS), \
    @command -v hr >/dev/null 2>&1 && hr $(GHC_VERSION) || true $(newline) \
    @make test GHC_VERSION=$(GHC_VERSION) $(newline) \
  )
else
> $(foreach CONFIG,$(STACK_TEST_CONFIGS), \
    @command -v hr >/dev/null 2>&1 && hr $(CONFIG) || true $(newline) \
    @make test CONFIG=$(CONFIG) $(newline) \
  )
endif
.PHONY: test-all

test-nightly: # run tests for the latest Stackage nightly release
ifeq ($(MODE), cabal)
> $(error test-nightly not supported in CABAL mode)
endif
> @make test RESOLVER=nightly
.PHONY: test-nightly

todo: # search for TODO items
> @find . -type f \
>   -not -path '*/\.*' \
>   -not -path './build/*' \
>   -not -path './project/*' \
>   -not -path ./Makefile \
>   | xargs grep -Hn TODO \
>   | grep -v '^Binary file ' \
>   || true
.PHONY: todo

version: # show current version
> @echo "$(PROJECT) $(call get_version)"
.PHONY: version
