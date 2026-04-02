LIBRARIES = elm/core/1.0.5 elmcraft/core-extra/2.2.0 elm/json/1.1.4 elm/regex/1.0.0 elm/parser/1.1.0 elm/bytes/1.0.8

.PHONY: all
all: generated/Core/Basics.elm

KERNEL_PATH = build/src/elm/kernel/0.0.0

CORE_SRC_OVERRIDES = build/src/elm/core/1.0.5/src/Task.elm

generated/Core/Basics.elm: codegen/Gen/Basics.elm codegen/Generate.elm node_modules/elm-codegen/bin/elm-codegen $(patsubst %,build/src/%/elm.json,$(LIBRARIES)) $(KERNEL_PATH)/src/Elm/Kernel/List.elm $(CORE_SRC_OVERRIDES)
	yarn elm-codegen run --flags-from build/src

codegen/Gen/Basics.elm: codegen/elm.codegen.json node_modules/elm-codegen/bin/elm-codegen $(wildcard helpers/*.elm)
	yarn elm-codegen install

node_modules/elm-codegen/bin/elm-codegen: package.json yarn.lock
	yarn install
	touch -c $@

.PRECIOUS: build/%.tar.gz
build/%.tar.gz:
	set -e &&\
	NAME=$$(echo $* | cut -d/ -f1,2) &&\
	VERSION=$$(echo $* | cut -d/ -f3) &&\
	mkdir -p $(dir $@) &&\
	curl -sSL https://github.com/$$NAME/archive/refs/tags/$$VERSION.tar.gz -o $@

build/src/%/elm.json: build/%.tar.gz
	mkdir -p $(@D)
	tar -xf $< --strip-components=1 -C $(@D) -m

$(KERNEL_PATH)/src/Elm/Kernel/List.elm: $(wildcard codegen/Elm/Kernel/*.elm)
	mkdir -p $(KERNEL_PATH)/src/Elm
	cp -r codegen/Elm/Kernel $(KERNEL_PATH)/src/Elm/

# Copy source overrides (e.g. simplified Task.elm) into the downloaded elm/core tree
# so the codegen picks them up instead of the originals
build/src/elm/core/1.0.5/src/Task.elm: codegen/Elm/src/Task.elm build/src/elm/core/1.0.5/elm.json
	cp $< $@

ALL_GENERATED = $(shell find generated -type f -name '*.elm')
ALL_SRC = $(shell find src -type f -name '*.elm')
dist/ui.js: src/UI.elm $(ALL_SRC) generated/Core/Basics.elm $(ALL_GENERATED)
	elm make $< --output $@

.PHONY: measure
measure: dist/ui.js
	du -sh $^
	gzip -9 $^
	du -sh $^.gz
	gunzip $^
	npx elmjs-inspect $^ | head -10
