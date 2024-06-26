##############################################################################
# Project configuration

PACKAGE     := lsupg
CABAL_FILE  := $(PACKAGE).cabal
PROJECT     := $(PACKAGE)-haskell
EXECUTABLES := lsupg

MODE ?= stack

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
else ifeq ($(MODE), stack)
  STACK_ARGS :=
  ifneq ($(origin CONFIG), undefined)
    STACK_ARGS += --stack-yaml "$(CONFIG)"
  endif
  ifneq ($(origin RESOLVER), undefined)
    STACK_ARGS += --resolver "$(RESOLVER)"
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
> cabal v2-build $(CABAL_ARGS) --enable-tests --enable-benchmarks
else
> stack build $(STACK_ARGS) --test --bench --no-run-tests --no-run-benchmarks
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
> @rm -rf .stack-work
> @rm -rf build
> @rm -rf dist-newstyle
> @rm -f *.yaml.lock
> @rm -f cabal.project.local
> @rm -f result*
.PHONY: clean-all

coverage: hr
coverage: # run tests with code coverage *
ifeq ($(MODE), cabal)
> cabal v2-test $(CABAL_ARGS) \
>   --enable-coverage --enable-tests --test-show-details=always
else
> stack test $(STACK_ARGS) --coverage
> stack hpc report .
endif
.PHONY: coverage

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
> @if command -v column >/dev/null 2>&1 \
>  ; then \
>    grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
>    | sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
>    | column -t -s $$'\t' \
>  ; else \
>    grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
>    | sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
>  ; fi
> @echo
> @echo "Cabal mode (MODE=cabal)"
> @echo "  * Set GHC_VERSION to specify a GHC version."
> @echo "  * Set PROJECT_FILE to specify a cabal.project file."
> @echo
> @echo "Stack mode (MODE=stack)"
> @echo "  * Set CONFIG to specify a stack.yaml file."
> @echo "  * Set RESOLVER to specify a Stack resolver."
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

ignored: # list files ignored by git
> @git ls-files . --ignored --exclude-standard --others
.PHONY: ignored

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

static: hr
static: # build a static executable *
> $(eval VERSION := $(call get_version))
ifeq ($(MODE), cabal)
> $(eval REPO := "$(PACKAGE)-build:$(GHC_VERSION)")
> @docker image inspect "$(REPO)" >/dev/null 2>&1 \
>   || docker buildx build \
>        --build-arg "GHC_VERSION=$(GHC_VERSION)" \
>        -t "$(REPO)" \
>        -f docker/Dockerfile \
>        .
> @rm -rf dist-newstyle
> docker run --rm -it -v "$(CURDIR):/host" "$(REPO)" \
>   "/host/bin/build-cabal-static.sh" --flags=static $(CABAL_ARGS)
> @mkdir -p "build"
> $(foreach EXE,$(EXECUTABLES), \
    @cp "$$(find dist-newstyle -type f -name $(EXE))" "build" $(newline) \
    @strip "build/$(EXE)" $(newline) \
  )
> @cd build && tar -Jcvf "$(PACKAGE)-$(VERSION).tar.xz" $(EXECUTABLES)
else
ifeq ($(origin CONFIG), undefined)
> $(eval CONFIG := stack.yaml)
endif
> $(eval REPO := $(shell grep '^ *repo: "[^"]*"$$' "$(CONFIG)" \
    | sed -e 's/^[^"]*"//' -e 's/"$$//'))
> $(eval GHC_VERSION := $(shell echo "$(REPO)" | sed 's/^[^:]*://' ))
> $(eval FLAGS := --flag "$(PACKAGE):static")
> $(eval FLAGS += $(shell bin/stack-yaml-flags "$(CONFIG)"))
> @docker image inspect "$(REPO)" >/dev/null 2>&1 \
>   || docker buildx build \
>        --build-arg "GHC_VERSION=$(GHC_VERSION)" \
>        -t "$(REPO)" \
>        -f docker/Dockerfile \
>        .
> @rm -rf .stack-work
> stack build --stack-yaml $(CONFIG) $(FLAGS) --docker
> @mkdir -p "build"
> $(foreach EXE,$(EXECUTABLES), \
    @cp "$$(find .stack-work/install -type f -name $(EXE))" "build" \
    $(newline) \
  )
> @cd build && tar -Jcvf "$(PACKAGE)-$(VERSION).tar.xz" $(EXECUTABLES)
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

test-all: # run all configured tests using MODE
> @./test-all.sh "$(MODE)"
.PHONY: test-all

test-bounds-lower: # test lower bounds (Cabal only)
ifeq ($(MODE), stack)
> $(error test-bounds-lower not supported in Stack mode)
endif
> @make test-build CABAL_ARGS="--project-file=cabal-bounds-lower.project"
.PHONY: test-bounds-lower

test-bounds-upper: # test upper bounds (Cabal only)
ifeq ($(MODE), stack)
> $(error test-bounds-upper not supported in Stack mode)
endif
> @make test-build CABAL_ARGS="--project-file=cabal-bounds-upper.project"
.PHONY: test-bounds-upper

test-build: hr
test-build: build
test-build: test
test-build: doc-api
test-build: # build, run tests, build API documentation *
.PHONY: test-build

test-nightly: # run tests for the latest Stackage nightly release (Stack only)
ifeq ($(MODE), cabal)
> $(error test-nightly not supported in Cabal mode)
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
